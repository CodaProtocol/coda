package delegation_backend

import (
  "github.com/btcsuite/btcutil/base58"
  "math/rand"
  "testing"
  "testing/quick"
  "reflect"
  "encoding/json"
)

var STR_ALPHABET = []rune("0123456789[]{};'/.,~abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
const B58_ALPHABET = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

func randBytes(n int, r *rand.Rand) []byte {
  b := make([]byte, n)
  _, _ = r.Read(b)
  return b
}

func randB58(n int, ver byte, r *rand.Rand) ([]byte, string) {
  bs := randBytes(n, r)
  return bs, base58.CheckEncode(bs, ver)
}

func randB58WithWrongCheck(n int, ver byte, r *rand.Rand) string {
  var v2 string
  _, v2 = randB58(PK_LENGTH, BASE58CHECK_VERSION_PK, r)
  return randReplaceOneChar(v2, B58_ALPHABET, r)
}

func randReplaceOneChar(s string, alphabet string, r *rand.Rand) string {
  bs := []byte(s)
  i := r.Intn(len(bs))
  bs[i] = randOther(bs[i], func () interface{} {
    return alphabet[r.Intn(len(alphabet))]
  }, r).(byte)
  return string(bs)
}

func randOther(orig interface{}, gen func()interface{}, r *rand.Rand) interface{} {
  for {
    c := gen()
    if orig != c {
      return c
    }
  }
}

func randString(r *rand.Rand) string {
  n := r.Intn(100)
  b := make([]rune, n)
  for i := range b {
    b[i] = STR_ALPHABET[r.Intn(len(STR_ALPHABET))]
  }
  return string(b)
}

type PkJSONUnmarshalTestData struct {
  good string
  bad []string
  pk Pk
}

type SigJSONUnmarshalTestData struct {
  good string
  bad []string
  sig Sig
}

func genB58JSONUnmarshalData (bs []byte, ver byte, r *rand.Rand) (string, []string) {
  good := base58.CheckEncode(bs, ver)
  rv := randOther(ver, func () interface{} {
    return byte(r.Uint32()%256)
  }, r).(byte)
  bad1 := base58.CheckEncode(bs, rv)
  bad2 := randReplaceOneChar(good, B58_ALPHABET, r)
  bad3 := randReplaceOneChar(bad1, B58_ALPHABET, r)
  l := randOther(len(bs), func () interface{} {
    return r.Intn(1000)+1
  }, r).(int)
  _, bad4 := randB58(l, ver, r)
  return good, []string{ bad1, bad2, bad3, bad4 }
}

func (PkJSONUnmarshalTestData) Generate(r *rand.Rand, size int) reflect.Value {
  var pk Pk
  r.Read(pk[:])
  pkBytes := append([]byte{1,1}, pk[:]...)
  good, bad := genB58JSONUnmarshalData(pkBytes, BASE58CHECK_VERSION_PK, r)
  return reflect.ValueOf(PkJSONUnmarshalTestData{good, bad, pk})
}

func (SigJSONUnmarshalTestData) Generate(r *rand.Rand, size int) reflect.Value {
  var sig Sig
  r.Read(sig[:])
  good, bad := genB58JSONUnmarshalData(append([]byte{1}, sig[:]...), BASE58CHECK_VERSION_SIG, r)
  return reflect.ValueOf(SigJSONUnmarshalTestData{good, bad, sig})
}

func TestPkJSONUnmarshal(t *testing.T) {
  f := func(td PkJSONUnmarshalTestData) bool {
    var v Pk
    b := json.Unmarshal([]byte("\"" + td.good + "\""), &v) == nil && v == td.pk
    for _, bad := range(td.bad) {
      b = b && json.Unmarshal([]byte("\"" + bad + "\""), &v) != nil
    }
    return b
  }
  if err := quick.Check(f, nil); err != nil {
    t.Error(err)
  }
}

func TestSigJSONUnmarshal(t *testing.T) {
  f := func(td SigJSONUnmarshalTestData) bool {
    var v Sig
    b := json.Unmarshal([]byte("\"" + td.good + "\""), &v) == nil && v == td.sig
    for _, bad := range(td.bad) {
      b = b && json.Unmarshal([]byte("\"" + bad + "\""), &v) != nil
    }
    return b
  }
  if err := quick.Check(f, nil); err != nil {
    t.Error(err)
  }
}
