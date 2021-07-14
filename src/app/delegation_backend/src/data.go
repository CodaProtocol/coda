package delegation_backend

import (
  "fmt"
  "errors"
  "bytes"
  "time"
  "encoding/json"
  "github.com/btcsuite/btcutil/base58"
  "encoding/base64"
)

// Just a different interface for Unmarshal
func JSONToString (b []byte) (s string, err error) {
  err = json.Unmarshal(b, &s)
  return
}
func StringToPk (pk *Pk, s string) error {
  bs, ver, err := base58.CheckDecode(s)
  if err == nil && ver != BASE58CHECK_VERSION_PK {
    return errors.New("Unexpected base58check version for Pk")
  }
  if err == nil {
    prefixLen := len(PK_PREFIX)
    if len(bs) == PK_LENGTH + prefixLen {
      if bytes.Equal(bs[:prefixLen], PK_PREFIX[:]) {
        copy(pk[:], bs[prefixLen:])
      } else {
        err = errors.New("Unexpected prefix of Pk")
      }
    } else {
      err = errors.New(fmt.Sprintf("Public key of an unexpected size %d", len(bs)))
    }
  }
  return err
}
func StringToSig (sig *Sig, s string) error {
  bs, ver, err := base58.CheckDecode(s)
  if err == nil && ver != BASE58CHECK_VERSION_SIG {
    return errors.New("Unexpected base58check version for Sig")
  }
  if err == nil {
    prefixLen := len(SIG_PREFIX)
    if len(bs) == SIG_LENGTH + prefixLen {
      if bytes.Equal(bs[:prefixLen], SIG_PREFIX[:]) {
        copy(sig[:], bs[prefixLen:])
      } else {
        err = errors.New("Unexpected prefix of signature")
      }
    } else {
      err = errors.New(fmt.Sprintf("Signature of an unexpected size %d", len(bs)))
    }
  }
  return err
}

type Sig [SIG_LENGTH]byte
func (sig *Sig) UnmarshalJSON (b []byte) error {
  s, err := JSONToString(b)
  if err == nil {
    err = StringToSig(sig, s)
  }
  return err
}
func (d Sig) MarshalJSON() ([]byte, error) {
  return json.Marshal(base58.CheckEncode(append(SIG_PREFIX[:], d[:]...), BASE58CHECK_VERSION_SIG))
}

type Pk [PK_LENGTH]byte
func (pk *Pk) UnmarshalJSON (b []byte) error {
  s, err := JSONToString(b)
  if err == nil {
    err = StringToPk(pk, s)
  }
  return err
}
func (d Pk) MarshalJSON() ([]byte, error) {
  return json.Marshal(base58.CheckEncode(append(PK_PREFIX[:], d[:]...), BASE58CHECK_VERSION_PK))
}
func (pk Pk) Format() string {
  return pk.String()
}
func (pk Pk) String() string {
  return base58.CheckEncode(pk[:], BASE58CHECK_VERSION_PK)
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

type BufferOrError struct {
  Buf bytes.Buffer
  Err error
}

func (boe *BufferOrError) WriteString(s string){
  if boe.Err == nil {
    _, err := boe.Buf.WriteString(s)
    boe.Err = err
  }
}

func (boe *BufferOrError) Write(b []byte){
  if boe.Err == nil {
    _, err := boe.Buf.Write(b)
    boe.Err = err
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

