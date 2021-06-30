package delegation_backend

import (
  "io"
  "errors"
  "bytes"
  "strings"
  "context"
  "golang.org/x/crypto/blake2b"
  "net/http"
  "time"
  "encoding/json"
  logging "github.com/ipfs/go-log/v2"
  "github.com/btcsuite/btcutil/base58"
  "encoding/base64"
  storage "cloud.google.com/go/storage"
)

const MAX_SUBMIT_PAYLOAD_SIZE = 1000000
const MAX_BLOCK_SIZE = 50000000
const REQUESTS_PER_PK_HOURLY = 120
const DELEGATION_BACKEND_LISTEN_TO = ":8080"
const TIME_DIFF_DELTA time.Duration = -5*60*1000000000 // -5m

const BASE58CHECK_VERSION_BLOCK_HASH byte = 0x10
const BASE58CHECK_VERSION_PK byte = 0xCB
const BASE58CHECK_VERSION_SIG byte = 0x9A

type unit = struct{}

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
  Context *context.Context
}

type SubmitH struct {
  app *App
}

func Base58CheckUnmarshalJSON (ver byte, b []byte) ([]byte, error) {
  var s string
  if err := json.Unmarshal(b, &s); err != nil {
    return nil, err
  }
  bs, ver_, err := base58.CheckDecode(s)
  if err != nil {
    return nil, err
  }
  if ver != ver_ {
    return nil, errors.New("Unexpected base58check version")
  }
  return bs, err
}

type Sig struct {
  data []byte
}
func (d *Sig) UnmarshalJSON (b []byte) error {
  bs, err := Base58CheckUnmarshalJSON(BASE58CHECK_VERSION_SIG, b)
  if err == nil {
    d.data = bs
  }
  return err
}

type Pk struct {
  data []byte
}
func (d *Pk) UnmarshalJSON (b []byte) error {
  bs, err := Base58CheckUnmarshalJSON(BASE58CHECK_VERSION_PK, b)
  if err == nil {
    d.data = bs
  }
  return err
}

type Base64 struct {
  data []byte
  json []byte
}
func (d *Base64) UnmarshalJSON (b []byte) error {
  var s string
  if err := json.Unmarshal(b, &s); err != nil {
    return err
  }
  bs, err := base64.StdEncoding.DecodeString(s)
  if err == nil {
    d.data = bs
    d.json = b
  }
  return err
}
func (d *Base64) MarshalJSON() ([]byte, error) {
  return d.json, nil
}

type submitRequestData struct {
  peerId *Base64 `json:"peer_id"`
  block *Base64 `json:"block"`
  snarkWork *Base64 `json:"snark_work,omitempty"`
  createdAt time.Time `json:"created_at"`
}
type submitRequest struct {
  data submitRequestData `json:"data"`
  submitter Pk `json:"submitter"`
  sig Sig `json:"sig"`
}
type metaToBeSaved struct {
  submittedAt time.Time `json:"submitted_at"`
  peerId *Base64 `json:"peer_id"`
  snarkWork *Base64 `json:"snark_work,omitempty"`
  remoteAddr string `json:"remote_addr"`
}

type BlockHash struct {
  data [blake2b.Size256]byte
  str string
}

type BufferOrError struct {
  buf bytes.Buffer
  err error
}

func (boe *BufferOrError) WriteString(s string){
  if boe.err == nil {
    _, err := boe.buf.WriteString(s)
    boe.err = err
  }
}

func (boe *BufferOrError) Write(b []byte){
  if boe.err == nil {
    _, err := boe.buf.Write(b)
    boe.err = err
  }
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
  // TODO check that `submitter` didn't exceed the limits
  // - `429 Too Many Requests` when submission from public key `pk` is rejected due to rate-limiting policy
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
  pkStr := base58.CheckEncode(req.submitter.data, BASE58CHECK_VERSION_PK)
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
  // TODO save meta and block to google cloud
  
  pathMeta := strings.Join([]string{"submissions", pkStr, blockHash.str, createdAtStr + ".json"}, "/")

  metaO := h.app.Bucket.Object(pathMeta)
  // TODO check that this is how context is to be used
  metaW := metaO.NewWriter(*h.app.Context)

  // TODO should be a goroutine
  defer metaW.Close()
  io.Copy(metaW, bytes.NewReader(metaBytes))

  blockO := h.app.Bucket.Object("blocks/" + blockHash.str + ".dat")
  blockW := blockO.NewWriter(*h.app.Context)

  // TODO should be a goroutine
  defer blockW.Close()
  io.Copy(blockW, bytes.NewReader(req.data.block.data))

  w.Write([]byte("{\"status\":\"ok\"}"))
}

func (app *App) NewSubmitH() *SubmitH {
  s := new(SubmitH)
  s.app = app
  return s
}
