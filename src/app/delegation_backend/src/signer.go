package delegation_backend

// #cgo LDFLAGS: -L../result
// #cgo LDFLAGS: -lmina_signer -lm
// #cgo CFLAGS: -w -I ../result/headers
// #include "crypto.h"
import "C"
import "unsafe"

func verifySig(pk *Pk, sig *Sig, data []byte, networkId uint8) bool {
  pkC := C.CString(string(pk[:]))
  defer C.free(unsafe.Pointer(pkC))
  sigC := C.CString(string(sig[:]))
  defer C.free(unsafe.Pointer(sigC))
  dataC := C.CString(string(data))
  defer C.free(unsafe.Pointer(dataC))
  return bool(C.verify_string(sigC, pkC, dataC, C.size_t(len(data)), C.uint8_t(networkId)))
}
