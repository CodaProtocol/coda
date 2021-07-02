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
  Msg string `json:"error"`
}

func writeErrorResponse (app *App, w *http.ResponseWriter, msg string) {
  bs, err := json.Marshal(errorResponse{msg})
  if err == nil {
    _, err2 := io.Copy(*w, bytes.NewReader(bs))
    if err2 != nil {
      app.Log.Debugf("Failed to respond with error status: %v", err2)
    }
  } else {
    app.Log.Fatal("Failed to json-marshal error message")
  }
}

type App struct {
  Log *logging.ZapEventLogger
  Bucket *storage.BucketHandle
  Context context.Context
  SubmitCounter *AttemptCounter
  Whitelist *WhitelistMVar
}

type SubmitH struct {
  app *App
}

func makeSignPayload (req *submitRequestData) (*BlockHash, []byte, error) {
  blockHash := new(BlockHash)
  blockHash.data = blake2b.Sum256(req.Block.data)
  blockHash.str = base58.CheckEncode(blockHash.data[:], BASE58CHECK_VERSION_BLOCK_HASH)
  createdAtStr := req.CreatedAt.Format(time.RFC3339)
  createdAtJson, err2 := json.Marshal(createdAtStr)
  if err2 != nil {
    return blockHash, nil, err2
  }
  signPayload := new(BufferOrError)
  signPayload.WriteString("{\"block\":")
  signPayload.Write(req.Block.json)
  signPayload.WriteString(",\"created_at\":")
  signPayload.Write(createdAtJson)
  signPayload.WriteString(",\"peer_id\":")
  signPayload.Write(req.PeerId.json)
  if req.SnarkWork != nil {
    signPayload.WriteString(",\"snark_work\":")
    signPayload.Write(req.SnarkWork.json)
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

  wl := h.app.Whitelist.ReadWhitelist()
  if (*wl)[req.Submitter] == nil {
    w.WriteHeader(401)
    writeErrorResponse(h.app, &w, "Submitter is not registered")
    return
  }

  passesAttemptLimit := h.app.SubmitCounter.RecordAttempt(req.Submitter)
  if !passesAttemptLimit {
    w.WriteHeader(429)
    writeErrorResponse(h.app, &w, "Too many requests per hour")
    return
  }

  submittedAt := time.Now()
  if req.Data.CreatedAt.Add(TIME_DIFF_DELTA).After(submittedAt) {
    h.app.Log.Debugf("Field created_at is a timestamp in future: %v", submittedAt)
    w.WriteHeader(400)
    writeErrorResponse(h.app, &w, "Field created_at is a timestamp in future")
    return
  }
  if req.Data.Block == nil || req.Data.PeerId == nil {
    h.app.Log.Debug("One of required fields wasn't provided")
    w.WriteHeader(400)
    writeErrorResponse(h.app, &w, "One of required fields wasn't provided")
    return
  }
  blockHash, payload, err := makeSignPayload(&req.Data)
  if err != nil {
    h.app.Log.Errorf("Error while unmarshaling JSON of /submit request's body: %v", err)
    w.WriteHeader(500)
    writeErrorResponse(h.app, &w, "Unexpected server error")
    return
  }
  h.app.Log.Debugf("Prepared signing payload: %v", string(payload))
  // TODO check signatures
  pkStr := base58.CheckEncode(req.Submitter[:], BASE58CHECK_VERSION_PK)
  createdAtStr := req.Data.CreatedAt.Format(time.RFC3339)

  var meta metaToBeSaved
  meta.SubmittedAt = submittedAt
  meta.PeerId = req.Data.PeerId
  meta.SnarkWork = req.Data.SnarkWork
  meta.RemoteAddr = r.RemoteAddr

  metaBytes, err1:= json.Marshal(meta)
  if err1 != nil {
    h.app.Log.Errorf("Error while marshaling JSON for metaToBeSaved: %v", err)
    w.WriteHeader(500)
    writeErrorResponse(h.app, &w, "Unexpected server error")
    return
  }

  pathMeta := strings.Join([]string{"submissions", pkStr, blockHash.str, createdAtStr + ".json"}, "/")

  metaO := h.app.Bucket.Object(pathMeta)
  blockO := h.app.Bucket.Object("blocks/" + blockHash.str + ".dat")

  go func(){
    metaW := metaO.NewWriter(h.app.Context)
    defer metaW.Close()
    _, err3 := io.Copy(metaW, bytes.NewReader(metaBytes))
    if err3 != nil {
      h.app.Log.Debugf("Error while saving metadata: %v", err3)
      return
    }
    blockW := blockO.NewWriter(h.app.Context)
    defer blockW.Close()
    _, err4 := io.Copy(blockW, bytes.NewReader(req.Data.Block.data))
    if err4 != nil {
      h.app.Log.Debugf("Error while saving block: %v", err4)
    }
  }()

  _, err2 := io.Copy(w, bytes.NewReader([]byte("{\"status\":\"ok\"}")))
  if err2 != nil {
    h.app.Log.Debugf("Error while responding with ok status to the user: %v", err2)
  }
}

func (app *App) NewSubmitH() *SubmitH {
  s := new(SubmitH)
  s.app = app
  return s
}
