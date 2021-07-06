package delegation_backend

import (
  "time"
  "sync"
  "math/rand"
  "testing"
)

const s time.Duration = 1000000000
const m time.Duration = 60*s

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

func TestZeroAttempt(t *testing.T) {
  counter, mock := newTestAttemptCounter(0)
  pk := mkPk()
  if counter.RecordAttempt(pk) {
    t.FailNow()
  }
  if counter.RecordAttempt(pk) {
    t.FailNow()
  }
  mock.Advance(60*m)
  if counter.RecordAttempt(pk) {
    t.FailNow()
  }
}
