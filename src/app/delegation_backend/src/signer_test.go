package delegation_backend

import (
  "encoding/json"
  "testing"
)

func testVerifySig(pkStr string, sigStr string, data []byte, t *testing.T) {
  var pk Pk
  var sig Sig
  if err := StringToSig(&sig, sigStr); err != nil {
    t.Logf("Error parsing sig: %s", sigStr)
    t.FailNow()
  }
  if err := StringToPk(&pk, pkStr); err != nil {
    t.Logf("Error parsing pk: %s", pkStr)
    t.FailNow()
  }
  if !verifySig(&pk, &sig, data, 1) {
    t.FailNow()
  }
}

const PK1 = "B62qkaKV3BLvLTf7nYXRehSaZAd36NWijt3MEmy2QgHRavboeRGtBMN"
const SIG1 =  "7mX7qCqaB8G5ry12EkqY9fcE6np8sdF1Kg3Hac77bRhwGq5PTBLEiNveEXn7oLgFs1dmxXyGwn5iCunBhamjNjrzNEdoJhwd"
func TestVerifySig1 (t *testing.T) {
  testVerifySig(PK1, SIG1, []byte{0, 0}, t)
}

const PK2 = "B62qn4kByk45GFDMHwVf2smYEVTehGN638UBHMaQzyLJf52wtdtCjRA"
const SIG2 = "7mXTf2BWNYSPuKGa3LT4ywcNH4EAqY5YoaasShG4Dpm44oJZMu1BZmVuGGqeDbfxUhvt2UcPZgSSQcv567dFueK22JoHndk9"
func TestVerifySig2 (t *testing.T) {
  testVerifySig(PK2, SIG2, []byte{1}, t)
}

const PK3 = "B62qkgvrx8WoDqTzXaUm2vqBRbkiqTts35Z9n4XcrFez1KG3MHKHTbn"
const SIG3 = "7mX6xdEWghp5vhuV33h1o4n8PQ8RYYMGNEwUXxk3rPm8inZ72Yxjw9zhvc3mtkFPnGAbqy3rxBYXdFrVBt9YbKzRKZ1RLLeC"
func TestVerifySig3 (t *testing.T) {
  testVerifySig(PK3, SIG3, []byte{0xFF}, t)
}

func testVerifyRequest (name string, t *testing.T) {
  body := readTestFile(name, t)
  var req submitRequest
  if err := json.Unmarshal(body, &req); err != nil {
    t.Log("failed decoding test file")
    t.FailNow()
  }
  j, err := makeSignPayload(&req.Data)
  if err != nil {
    t.FailNow()
  }
  if !verifySig(&req.Submitter, &req.Sig, j, 1) {
    t.FailNow()
  }
}

func TestVerifyRequest1 (t *testing.T) {
  testVerifyRequest("req-with-snark-3", t)
}

func TestVerifyRequest2 (t *testing.T) {
  testVerifyRequest("req-with-snark-2", t)
}

const PK4 = "B62qoJC4KuLXgTEX2uwQGPNZSnqRTvJHzcEzkWTDFTXMsqdXPNKxJLs"
const SIG4 = "7mXR2NWjqPByGLXkPjY9JzZ5JFgcytcUXoKNT34Rpdzu6mUL6EB1gEyv8Q2Q7WCwczRELsCLsvjLdDRZBtB8gmQKHyKY5pSt"
func TestVerifySig4 (t *testing.T) {
  body := readTestFile("payload-1", t)
  testVerifySig(PK4, SIG4, body, t)
}

const PK5 = "B62qquUz9Pg3JuwjQ2TjLntr8kpqkNqfqsmVqFAPKVLt6KdiSMuyAbu"
const SIG5 = "7mX5TcnVKRjveT9qWQx4sY2258SzHfngvUSmqSAHzLe3uX9yH85EN8SVs6B8UmhDT5Z2aS96k9ELVNBiTP5p9iU6Q3ynPh8D"
func TestVerifySig5 (t *testing.T) {
  testVerifySig(PK5, SIG5, []byte{0x1, 0x23, 0x45, 0x67, 0x89, 0xAB, 0xCD, 0xEF}, t)
}
