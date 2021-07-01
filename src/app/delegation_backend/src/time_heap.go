package delegation_backend

import (
  "container/heap"
  "time"
  "sync"
)

const minusOneHour time.Duration = -60*60*1000000000

type timeHeap []time.Time

type AttemptCounter struct {
  attempts map[Pk]*timeHeap
  maxAttempt int
  mutex sync.Mutex
}

func (h timeHeap) Len() int {
  return len(h)
}
func (h timeHeap) Less(i, j int) bool {
  return h[j].After(h[i])
}
func (h timeHeap) Swap(i, j int) {
  h[i], h[j] = h[j], h[i]
}

func (h *timeHeap) Push(x interface{}) {
  // Push and Pop use pointer receivers because they modify the slice's length,
  // not just its contents.
  *h = append(*h, x.(time.Time))
}

func (h *timeHeap) Pop() interface{} {
  old := *h
  n := len(old)
  x := old[n-1]
  *h = old[0 : n-1]
  return x
}

func NewAttemptCounter (maxAttemptPerHour int) *AttemptCounter {
  th := new(AttemptCounter)
  th.maxAttempt = maxAttemptPerHour
  th.attempts = make(map[Pk]*timeHeap)
  return th
}

// Record attempt to access the service
// Returns `true` if attempt was successfully recorded
// or `false` if amount of attempts per Pk per hour exceeded.
func (h *AttemptCounter) RecordAttempt (pk Pk) bool {
  h.mutex.Lock()
  defer h.mutex.Unlock()
  curTime := time.Now()
  if h.attempts[pk] == nil {
    t := timeHeap(make([]time.Time, 0, h.maxAttempt))
    h.attempts[pk] = &t
  }
  t := h.attempts[pk]
  for {
    if ; len(*t) == 0 || (*t)[0].After(curTime.Add(minusOneHour)) {
      break
    }
    _ = heap.Pop(t)
  }
  if len(*t) >= h.maxAttempt {
    return false
  }
  heap.Push(t, curTime)
  return true
}
