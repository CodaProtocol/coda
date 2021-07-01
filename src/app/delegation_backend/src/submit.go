package delegation_backend

import (
  "io"
  "bytes"
  "strings"
  "context"
  "golang.org/x/crypto/blake2b"
  "net/http"
  "time"
  "encoding/json"
  logging "github.com/ipfs/go-log/v2"
  "github.com/btcsuite/btcutil/base58"
  "cloud.google.com/go/storage"
)

type errorResponse struct {
  msg string `json:"error"`
}

func writeErrorResponse (app *App, w *http.ResponseWriter, msg string) {
  resp := new(errorResponse)
  resp.msg = msg
  bytes, err := json.Marshal(resp)
  if err == nil {
    _, _ = (*w).Write(bytes)
  } else {
    app.Log.Fatal("Failed to json-marshal error message")
  }
}

type App struct {
  Log *logging.ZapEventLogger
  Bucket *storage.BucketHandle
  Context context.Context
  SubmitCounter *AttemptCounter
}

type SubmitH struct {
  app *App
}

func makeSignPayload (req *submitRequestData) (*BlockHash, []byte, error) {
  blockHash := new(BlockHash)
  blockHash.data = blake2b.Sum256(req.block.data)
  blockHash.str = base58.CheckEncode(blockHash.data[:], BASE58CHECK_VERSION_BLOCK_HASH)
  blockHashJson, err1 := json.Marshal(blockHash.str)
  if err1 != nil {
    return blockHash, nil, err1
  }
  createdAtStr := req.createdAt.Format(time.RFC3339)
  createdAtJson, err2 := json.Marshal(createdAtStr)
  if err2 != nil {
    return blockHash, nil, err2
  }
  signPayload := new(BufferOrError)
  signPayload.WriteString("{\"block_hash\":")
  signPayload.Write(blockHashJson)
  signPayload.WriteString(",\"created_at\":")
  signPayload.Write(createdAtJson)
  signPayload.WriteString(",\"peer_id\":")
  signPayload.Write(req.peerId.json)
  if req.snarkWork != nil {
    signPayload.WriteString(",\"snark_work\":")
    signPayload.Write(req.snarkWork.json)
  }
  signPayload.WriteString("}")
  return blockHash, signPayload.buf.Bytes(), signPayload.err
}

func (h *SubmitH) ServeHTTP(w http.ResponseWriter, r *http.Request) {
  if (r.ContentLength == -1) {
    w.WriteHeader(411)
    return
  } else if (r.ContentLength > MAX_SUBMIT_PAYLOAD_SIZE) {
    w.WriteHeader(413)
    return
  }
  body, err1 := io.ReadAll(io.LimitReader(r.Body, r.ContentLength))
  if (err1 != nil) {
    h.app.Log.Debugf("Error while reading /submit request's body: %v", err1)
    w.WriteHeader(400)
    writeErrorResponse(h.app, &w, "Unexpected EOF while reading the body")
    return
  }
  var req submitRequest
  if err := json.Unmarshal(body, &req); err != nil {
    h.app.Log.Debugf("Error while unmarshaling JSON of /submit request's body: %v", err)
    w.WriteHeader(400)
    writeErrorResponse(h.app, &w, "Wrong payload")
    return
  }
  // TODO check that `submitter` is whitelisted
  // - `401 Unauthorized`  when public key `pk` is not on the list of allowed keys

  passesAttemptLimit := h.app.SubmitCounter.RecordAttempt(req.submitter)
  if passesAttemptLimit {
    w.WriteHeader(429)
    writeErrorResponse(h.app, &w, "Too many requests per hour")
    return
  }

  submittedAt := time.Now()
  if req.data.createdAt.Add(TIME_DIFF_DELTA).After(submittedAt) {
    w.WriteHeader(400)
    writeErrorResponse(h.app, &w, "Field created_at is a timestamp in future")
    return
  }
  blockHash, payload, err := makeSignPayload(&req.data)
  if err != nil {
    h.app.Log.Errorf("Error while unmarshaling JSON of /submit request's body: %v", err)
    w.WriteHeader(500)
    writeErrorResponse(h.app, &w, "Unexpected server error")
    return
  }
  h.app.Log.Debugf("Prepared signing payload: %v", string(payload))
  pkStr := base58.CheckEncode(req.submitter[:], BASE58CHECK_VERSION_PK)
  createdAtStr := req.data.createdAt.Format(time.RFC3339)

  var meta metaToBeSaved
  meta.submittedAt = submittedAt
  meta.peerId = req.data.peerId
  meta.snarkWork = req.data.snarkWork
  meta.remoteAddr = r.RemoteAddr

  metaBytes, err1:= json.Marshal(meta)
  if err1 != nil {
    h.app.Log.Errorf("Error while marshaling JSON for metaToBeSaved: %v", err)
    w.WriteHeader(500)
    writeErrorResponse(h.app, &w, "Unexpected server error")
    return
  }

  pathMeta := strings.Join([]string{"submissions", pkStr, blockHash.str, createdAtStr + ".json"}, "/")

  metaO := h.app.Bucket.Object(pathMeta)
  metaW := metaO.NewWriter(h.app.Context)

  blockO := h.app.Bucket.Object("blocks/" + blockHash.str + ".dat")
  blockW := blockO.NewWriter(h.app.Context)

  go func(){
    defer metaW.Close()
    defer blockW.Close()
    io.Copy(metaW, bytes.NewReader(metaBytes))
    io.Copy(blockW, bytes.NewReader(req.data.block.data))
  }()

  w.Write([]byte("{\"status\":\"ok\"}"))
}

func (app *App) NewSubmitH() *SubmitH {
  s := new(SubmitH)
  s.app = app
  return s
}
