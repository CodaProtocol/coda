package delegation_backend

import "sync"

type unit = interface{}
type Whitelist map[Pk]unit

type WhitelistMVar struct {
  whitelistMutex sync.RWMutex
  whitelistSet *Whitelist
}

func (mvar *WhitelistMVar) Replace (wl *Whitelist) {
  mvar.whitelistMutex.Lock()
  defer mvar.whitelistMutex.Unlock()
  mvar.whitelistSet = wl
}

func (mvar *WhitelistMVar) ReadWhitelist() (wl *Whitelist) {
  mvar.whitelistMutex.RLock()
  defer mvar.whitelistMutex.RUnlock()
  return mvar.whitelistSet
}
