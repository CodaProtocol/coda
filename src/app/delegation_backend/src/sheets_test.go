package delegation_backend

import (
  "math/rand"
  "testing"
  "testing/quick"
  "encoding/hex"
  "reflect"
)

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
      var bs []byte
      bs, v = randB58(PK_LENGTH, BASE58CHECK_VERSION_PK, r)
      var pk_ Pk
      copy(pk_[:], bs)
      pk = &pk_
    case 3: // Correct base58check-encoded bytestring with a random non-pk version byte
      ver := randOther(BASE58CHECK_VERSION_PK, func () interface{} {
        return byte(r.Uint32()%256)
      }, r).(byte)
      _, v = randB58(PK_LENGTH, ver, r)
    case 4: // Incorrect base58check-encoded (one of the symbols altered, so check won't succeed)
      v = randB58WithWrongCheck(PK_LENGTH, BASE58CHECK_VERSION_PK, r)
    case 8: // Correct base58check-encoded, wrong length
      l := randOther(PK_LENGTH, func () interface{} {
        return r.Intn(1000)+1
      }, r).(int)
      _, v = randB58(l, BASE58CHECK_VERSION_PK, r)
  }
  return [](interface{}){v}, pk
}

type Rows struct {
  rows [][](interface{})
  expected Whitelist
}

func (Rows) Generate(r *rand.Rand, size int) reflect.Value {
  res := make([][](interface{}), 0, size)
  wl := make(Whitelist)
  for j := 0; j < size; j++ {
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
