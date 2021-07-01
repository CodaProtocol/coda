package delegation_backend

import (
  "fmt"
  "errors"
  "bytes"
  "golang.org/x/crypto/blake2b"
  "time"
  "encoding/json"
  "github.com/btcsuite/btcutil/base58"
  "encoding/base64"
)

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

type Sig [SIG_LENGTH]byte
func (d *Sig) UnmarshalJSON (b []byte) error {
  bs, err := Base58CheckUnmarshalJSON(BASE58CHECK_VERSION_SIG, b)
  if err == nil {
    if len(bs) == SIG_LENGTH {
      copy(d[:], bs)
    } else {
      err = errors.New(fmt.Sprintf("Signature of an unexpected size %d", len(bs)))
    }
  }
  return err
}

type Pk [PK_LENGTH]byte
func (d *Pk) UnmarshalJSON (b []byte) error {
  bs, err := Base58CheckUnmarshalJSON(BASE58CHECK_VERSION_PK, b)
  if err == nil {
    if len(bs) == PK_LENGTH {
      copy(d[:], bs)
    } else {
      err = errors.New(fmt.Sprintf("Public key of an unexpected size %d", len(bs)))
    }
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

type submitRequestData struct {
  PeerId *Base64 `json:"peer_id"`
  Block *Base64 `json:"block"`
  SnarkWork *Base64 `json:"snark_work,omitempty"`
  CreatedAt time.Time `json:"created_at"`
}
type submitRequest struct {
  Data submitRequestData `json:"data"`
  Submitter Pk `json:"submitter"`
  Sig Sig `json:"signature"`
}
type metaToBeSaved struct {
  SubmittedAt time.Time `json:"submitted_at"`
  PeerId *Base64 `json:"peer_id"`
  SnarkWork *Base64 `json:"snark_work,omitempty"`
  RemoteAddr string `json:"remote_addr"`
}

