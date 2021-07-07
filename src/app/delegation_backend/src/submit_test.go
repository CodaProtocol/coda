package delegation_backend

import (
  "testing"
  "encoding/base64"
  "time"
  "bytes"
)

const TSPG_EXPECTED_1 = `{"block":"zLgvHQzxSh8MWlTjXK+cMA==","created_at":"2021-07-01T16:21:33Z","peer_id":"MLF0jAGTpL84LLerLddNs5M10NCHM+BwNeMxK78+"}`
const TSPG_EXPECTED_2 = `{"block":"zLgvHQzxSh8MWlTjXK+cMA==","created_at":"2021-07-01T16:21:33Z","peer_id":"MLF0jAGTpL84LLerLddNs5M10NCHM+BwNeMxK78+","snark_work":"Bjtox/3Yu4cT5eVCQz/JQ+P3Ce1JmCIE7N6b1MAa"}`

func mkB64(s string) *Base64 {
  r := new(Base64)
  r.json = []byte("\"" + s + "\"")
  r.data, _ = base64.StdEncoding.DecodeString(s)
  return r
}

func TestSignPayloadGeneration(t *testing.T){
  req := new(submitRequestData)
  req.PeerId = mkB64("MLF0jAGTpL84LLerLddNs5M10NCHM+BwNeMxK78+")
  req.Block = mkB64("zLgvHQzxSh8MWlTjXK+cMA==")
  req.CreatedAt, _ = time.Parse(time.RFC3339, "2021-07-01T19:21:33+03:00")
  _, json, err := makeSignPayload(req)
  t.Log(string(json))
  if err != nil || !bytes.Equal(json, []byte(TSPG_EXPECTED_1)) {
    t.FailNow()
  }
  req.SnarkWork = mkB64("Bjtox/3Yu4cT5eVCQz/JQ+P3Ce1JmCIE7N6b1MAa")
  _, json, err = makeSignPayload(req)
  if err != nil || !bytes.Equal(json, []byte(TSPG_EXPECTED_2)) {
    t.FailNow()
  }
}
