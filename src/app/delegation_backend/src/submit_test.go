package delegation_backend

import (
  logging "github.com/ipfs/go-log/v2"
  "testing"
  "encoding/base64"
  "encoding/json"
  "time"
  "bytes"
  "net/http/httptest"
  "io/ioutil"
  "math/rand"
)

const TSPG_EXPECTED_1 = `{"block":"zLgvHQzxSh8MWlTjXK+cMA==","created_at":"2021-07-01T16:21:33Z","peer_id":"MLF0jAGTpL84LLerLddNs5M10NCHM+BwNeMxK78+"}`
const TSPG_EXPECTED_2 = `{"block":"zLgvHQzxSh8MWlTjXK+cMA==","created_at":"2021-07-01T16:21:33Z","peer_id":"MLF0jAGTpL84LLerLddNs5M10NCHM+BwNeMxK78+","snark_work":"Bjtox/3Yu4cT5eVCQz/JQ+P3Ce1JmCIE7N6b1MAa"}`

func mkB64 (s string) *Base64 {
  r := new(Base64)
  r.json = []byte("\"" + s + "\"")
  r.data, _ = base64.StdEncoding.DecodeString(s)
  return r
}

func mkB64B (b []byte) *Base64 {
  r := new(Base64)
  r.json = []byte("\"" + base64.StdEncoding.EncodeToString(b) + "\"")
  r.data = b
  return r
}

func TestSignPayloadGeneration(t *testing.T){
  req := new(submitRequestData)
  req.PeerId = mkB64("MLF0jAGTpL84LLerLddNs5M10NCHM+BwNeMxK78+")
  req.Block = mkB64("zLgvHQzxSh8MWlTjXK+cMA==")
  req.CreatedAt, _ = time.Parse(time.RFC3339, "2021-07-01T19:21:33+03:00")
  json, err := makeSignPayload(req)
  t.Log(string(json))
  if err != nil || !bytes.Equal(json, []byte(TSPG_EXPECTED_1)) {
    t.FailNow()
  }
  req.SnarkWork = mkB64("Bjtox/3Yu4cT5eVCQz/JQ+P3Ce1JmCIE7N6b1MAa")
  json, err = makeSignPayload(req)
  if err != nil || !bytes.Equal(json, []byte(TSPG_EXPECTED_2)) {
    t.FailNow()
  }
}

func testSubmitH (maxAttempt int, initWl Whitelist) (*ObjectsToSave, *SubmitH, *timeMock) {
  storage := make(ObjectsToSave)
  log := logging.Logger("delegation backend test")
  app := new(App)
  app.Log = log
  app.Save = func(objs ObjectsToSave) {
    for path, value := range(objs) {
      storage[path] = value
    }
  }
  counter, tm := newTestAttemptCounter(1)
  app.SubmitCounter = counter
  app.Now = tm.Now
  wlMvar := new(WhitelistMVar)
  wlMvar.Replace(&initWl)
  app.Whitelist = wlMvar
  return &storage, app.NewSubmitH(), tm
}

func (sh *SubmitH) testRequest(body []byte) *httptest.ResponseRecorder {
  recorder := httptest.NewRecorder()
  req := httptest.NewRequest("POST", "http://127.0.0.1/v1/submit", bytes.NewReader(body))
  sh.ServeHTTP(recorder, req)
  return recorder
}

func readTestFile(n string, t *testing.T) []byte {
  body, err := ioutil.ReadFile("../test/data/" + n + ".json")
  if err != nil {
    t.Log("can not read test file")
    t.FailNow()
  }
  return body
}

func TestWrongLengthProvided(t *testing.T){
  body := readTestFile("req-no-snark", t)
  _, sh, _ := testSubmitH(1, Whitelist{})
  rep := httptest.NewRecorder()
  req := httptest.NewRequest("POST", "http://127.0.0.1/v1/submit", bytes.NewReader(body))
  req.ContentLength = req.ContentLength + 100
  sh.ServeHTTP(rep, req)
  if rep.Code != 400 {
    t.Log(rep)
    t.FailNow()
  }
}

func TestNoLengthProvided(t *testing.T){
  body := readTestFile("req-no-snark", t)
  _, sh, _ := testSubmitH(1, Whitelist{})
  rep := httptest.NewRecorder()
  req := httptest.NewRequest("POST", "http://127.0.0.1/v1/submit", bytes.NewReader(body))
  req.ContentLength = -1
  sh.ServeHTTP(rep, req)
  if rep.Code != 411 {
    t.Log(rep)
    t.FailNow()
  }
}

func TestTooLarge(t *testing.T){
  var req submitRequest
  if err := json.Unmarshal(readTestFile("req-no-snark", t), &req); err != nil {
    t.Log("failed decoding test file")
    t.FailNow()
  }
  block := make([]byte, MAX_SUBMIT_PAYLOAD_SIZE + 1)
  _, _ = rand.Read(block)
  req.Data.Block = mkB64B(block[:])
  body, err := json.Marshal(req)
  if err != nil {
    t.Log("failed encoding JSON body")
    t.FailNow()
  }
  _, sh, _ := testSubmitH(1, Whitelist{})
  rep := sh.testRequest(body)
  if rep.Code != 413 {
    t.Log(rep)
    t.FailNow()
  }
}

func TestUnauthorized(t *testing.T){
  body := readTestFile("req-no-snark", t)
  _, sh, _ := testSubmitH(1, Whitelist{})
  rep := sh.testRequest(body)
  if rep.Code != 401 {
    t.Log(rep)
    t.FailNow()
  }
}

func TestPkLimitExceeded(t *testing.T){
  body := readTestFile("req-with-snark-2", t)
  var req submitRequest
  if err := json.Unmarshal(body, &req); err != nil {
    t.Log("failed decoding test file")
    t.FailNow()
  }
  otherSubmitter := mkPk()
  _, sh, _ := testSubmitH(1, Whitelist{req.Submitter: true, otherSubmitter: true})
  rep := sh.testRequest(body)
  if rep.Code != 200 {
    t.Logf("Unexpected failure: %v", rep)
    t.FailNow()
  }
  rep2 := sh.testRequest(body)
  if rep2.Code != 429 {
    t.Log(rep2)
    t.FailNow()
  }
  req.Submitter = otherSubmitter
  body2, err := json.Marshal(req)
  if err != nil {
    t.Log("failed encoding JSON body")
    t.FailNow()
  }
  rep3 := sh.testRequest(body2)
  if rep3.Code == 429 {
    // TODO replace with `!= 400` after implementing signature checking
    t.Log(rep3)
    t.FailNow()
  }
}

func TestSuccess(t *testing.T){
  testNames := []string{"req-no-snark", "req-with-snark", "req-with-snark-2"}
  for _, f := range(testNames) {
    body := readTestFile(f, t)
    var req submitRequest
    if err := json.Unmarshal(body, &req); err != nil {
      t.Logf("failed decoding test file %s", f)
      t.FailNow()
    }
    objs, sh, tm := testSubmitH(1, Whitelist{req.Submitter: true})
    rep := sh.testRequest(body)
    if rep.Code != 200 {
      t.Logf("Failed testing %s: %v", f, rep)
      t.FailNow()
    }
    paths := makePaths(&req)
    var meta metaToBeSaved
    meta.SubmittedAt = tm.Now()
    meta.PeerId = req.Data.PeerId
    meta.SnarkWork = req.Data.SnarkWork
    meta.RemoteAddr = "192.0.2.1:1234"
    metaBytes, err2 := json.Marshal(meta)
    if err2 != nil || !bytes.Equal((*objs)[paths.metaPath], metaBytes) ||
        !bytes.Equal((*objs)[paths.blockPath], req.Data.Block.data) {
      t.Logf("Content check failed for %s", f)
      t.FailNow()
    }
  }
}

func Test400(t *testing.T){
  body := readTestFile("req-with-snark-2", t)
  var req submitRequest
  if err := json.Unmarshal(body, &req); err != nil {
    t.Log("failed decoding test file")
    t.FailNow()
  }
  _, sh, tm := testSubmitH(1, Whitelist{req.Submitter: true})
  //2. Malformed JSON
  if rep := sh.testRequest([]byte("{}")); rep.Code != 400 {
    t.Logf("Empty json test failed: %v", rep)
    t.FailNow()
  }
  if rep := sh.testRequest([]byte("~~not really a json~~")); rep.Code != 400 {
    t.Logf("Not a json test failed: %v", rep)
    t.FailNow()
  }
  //3. Block or peer id not provided
  req2 := req
  req2.Data.Block = nil
  body2, err2 := json.Marshal(req2)
  if rep := sh.testRequest(body2); err2 != nil || rep.Code != 400 {
    t.Log("No block test failed")
    t.FailNow()
  }
  req2 = req
  req2.Data.PeerId = nil
  body2, err2 = json.Marshal(req2)
  if rep := sh.testRequest(body2); err2 != nil || rep.Code != 400 {
    t.Log("No peerId test failed")
    t.FailNow()
  }
  // TODO 4. Invalid signatures
  //5. Created_at in the future
  // in 50 years this test will likely fail unless we update test files :D
  minusDur := -h*24*365*50
  tm.Advance(minusDur)
  if rep := sh.testRequest(body); rep.Code != 400 {
    t.Logf("Failed to test created_at in future: %v", rep)
    t.FailNow()
  }
}
