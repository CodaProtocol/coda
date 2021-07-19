package delegation_backend

import (
  "time"
  "sync"
  "math/rand"
  "testing"
  "testing/quick"
  "reflect"
)

const s time.Duration = 1000000000
const m time.Duration = 60*s
const h time.Duration = 60*m

type timeMock struct {
  mutex sync.RWMutex
  time time.Time
}

func (t *timeMock) Now() time.Time {
  t.mutex.RLock()
  defer t.mutex.RUnlock()
  return t.time
}

func (t *timeMock) Advance(dur time.Duration) {
  t.mutex.Lock()
  defer t.mutex.Unlock()
  t.time = t.time.Add(dur)
}

func newTestAttemptCounter (maxAttemptPerHour int) (*AttemptCounter, *timeMock) {
  th := new(AttemptCounter)
  th.maxAttempt = maxAttemptPerHour
  th.attempts = make(map[Pk]*timeHeap)
  tm := new(timeMock)
  tm.time = time.Now()
  th.now = func () time.Time { return tm.Now() }
  return th, tm
}

func mkPk() Pk {
  var a Pk
  rand.Read(a[:])
  return a
}

func TestZeroMaxAttempt(t *testing.T) {
  counter, mock := newTestAttemptCounter(0)
  pk := mkPk()
  if counter.RecordAttempt(pk) {
    t.FailNow()
  }
  if counter.RecordAttempt(pk) {
    t.FailNow()
  }
  mock.Advance(h)
  if counter.RecordAttempt(pk) {
    t.FailNow()
  }
}

type MaxAttempt int

func (MaxAttempt) Generate(r *rand.Rand, size int) reflect.Value {
  p := MaxAttempt(r.Intn(100) + 1)
  return reflect.ValueOf(p)
}

func TestManyAttempts(t *testing.T) {
  pk := mkPk()
  f := func(maxAttempt MaxAttempt) bool {
    counter, timeMock := newTestAttemptCounter(int(maxAttempt))
    mad := time.Duration(maxAttempt)
    gap := 50 * m / mad
    rem := h - gap * mad
    for j := 0; j < 2; j++ {
      for i := MaxAttempt(0); i < maxAttempt; i++ {
        if !counter.RecordAttempt(pk) {
          return false
        }
        timeMock.Advance(gap)
      }
      if counter.RecordAttempt(pk) {
        return false
      }
      timeMock.Advance(rem - s)
      if counter.RecordAttempt(pk) {
        return false
      }
      timeMock.Advance(s)
      if !counter.RecordAttempt(pk) {
        return false
      }
      if counter.RecordAttempt(pk) {
        return false
      }
      timeMock.Advance(h)
    }
    return true
  }
  if err := quick.Check(f, nil); err != nil {
    t.Error(err)
  }
}

func TestTwoPks(t *testing.T) {
  pk1 := mkPk()
  pk2 := mkPk()
  counter, timeMock := newTestAttemptCounter(1)
  if !counter.RecordAttempt(pk1) {
    t.FailNow()
  }
  if counter.RecordAttempt(pk1) {
    t.FailNow()
  }
  timeMock.Advance(30*m)
  if !counter.RecordAttempt(pk2) {
    t.FailNow()
  }
  if counter.RecordAttempt(pk2) {
    t.FailNow()
  }
  timeMock.Advance(30*m)
  if counter.RecordAttempt(pk2) {
    t.FailNow()
  }
  if !counter.RecordAttempt(pk1) {
    t.FailNow()
  }
}
