package delegation_backend

import (
  "github.com/btcsuite/btcutil/base58"
  "math/rand"
  "testing"
  "testing/quick"
  "encoding/hex"
  "reflect"
)

func randBytes(n int, r *rand.Rand) []byte {
  b := make([]byte, n)
  _, _ = r.Read(b)
  return b
}

var STR_ALPHABET = []rune("0123456789[]{};'/.,~abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
const B58_ALPHABET = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

func randString(r *rand.Rand) string {
  n := r.Intn(100)
  b := make([]rune, n)
  for i := range b {
    b[i] = STR_ALPHABET[r.Intn(len(STR_ALPHABET))]
  }
  return string(b)
}

func randRow(r *rand.Rand) ([](interface{}), *Pk) {
  var v (interface{})
  var pk *Pk
  switch i := r.Intn(10); i {
    case 0: // Random hexadecimal string
      v = hex.EncodeToString(randBytes(r.Intn(100)+1, r))
    case 1: // Random string starting with https://
      v = "https://" + randString(r)
    case 5: // Integer
      v = r.Int()
    case 6: // Empty cell
      v = ""
    case 7: // Empty row
      return [](interface{}){}, nil
    case 9: // Float
      v = r.Float64()
    case 2: // Correct base58check-encoded pk
      bs := randBytes(PK_LENGTH, r)
      var pk_ Pk
      copy(pk_[:], bs)
      v = base58.CheckEncode(bs, BASE58CHECK_VERSION_PK)
      pk = &pk_
    case 3: // Correct base58check-encoded bytestring with a random non-pk version byte
      randB := func () byte {
        return byte(r.Uint32()%256)
      }
      for {
        ver := randB()
        if ver != BASE58CHECK_VERSION_PK {
          bs := randBytes(PK_LENGTH, r)
          v = base58.CheckEncode(bs, ver)
          break
        }
      }
    case 4: // Incorrect base58check-encoded (one of the symbols altered, so check won't succeed)
      bs := randBytes(PK_LENGTH, r)
      v_ := []byte(base58.CheckEncode(bs, BASE58CHECK_VERSION_PK))
      randC := func () byte {
        return B58_ALPHABET[r.Intn(len(B58_ALPHABET))]
      }
      for {
        c := randC()
        i := r.Intn(len(v_))
        if v_[i] != c {
          v_[i] = c
          break
        }
      }
      v = string(v_)
    case 8: // Correct base58check-encoded, wrong length
      randL := func () int {
        return r.Intn(1000)+1
      }
      for {
        l := randL()
        if l != PK_LENGTH {
          bs := randBytes(l, r)
          v = base58.CheckEncode(bs, BASE58CHECK_VERSION_PK)
          break
        }
      }
  }
  return [](interface{}){v}, pk
}

type Rows struct {
  rows [][](interface{})
  expected Whitelist
}

func (Rows) Generate(r *rand.Rand, size int) reflect.Value {
  n := 1 //r.Intn(1000) + 10
  res := make([][](interface{}), 0, n)
  wl := make(Whitelist)
  for j := 0; j < n; j++ {
    row, pk := randRow(r)
    res = append(res, row)
    if pk != nil {
      wl[*pk] = true
    }
  }
  return reflect.ValueOf(Rows{res, wl})
}

func TestProcessRow(t *testing.T) {
  f := func(rows Rows) bool {
    actual := processRows(rows.rows)
    res := reflect.DeepEqual(map[Pk]unit(rows.expected), map[Pk]unit(actual))
    if !res {
      t.Logf("expected: %s", rows.expected)
      t.Logf("actual: %s", actual)
    }
    return res
  }
  if err := quick.Check(f, nil); err != nil {
    t.Error(err)
  }
}
