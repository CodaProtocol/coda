// generated by generate_methodidx; DO NOT EDIT

package main

import (
	"encoding/json"
	"fmt"
)

var (
	_methodIdxNameToValue = map[string]methodIdx{
		"configure":           configure,
		"listen":              listen,
		"publish":             publish,
		"subscribe":           subscribe,
		"unsubscribe":         unsubscribe,
		"validationComplete":  validationComplete,
		"generateKeypair":     generateKeypair,
		"openStream":          openStream,
		"closeStream":         closeStream,
		"resetStream":         resetStream,
		"sendStreamMsg":       sendStreamMsg,
		"removeStreamHandler": removeStreamHandler,
		"addStreamHandler":    addStreamHandler,
		"listeningAddrs":      listeningAddrs,
		"addPeer":             addPeer,
		"beginAdvertising":    beginAdvertising,
		"findPeer":            findPeer,
		"listPeers":           listPeers,
	}

	_methodIdxValueToName = map[methodIdx]string{
		configure:           "configure",
		listen:              "listen",
		publish:             "publish",
		subscribe:           "subscribe",
		unsubscribe:         "unsubscribe",
		validationComplete:  "validationComplete",
		generateKeypair:     "generateKeypair",
		openStream:          "openStream",
		closeStream:         "closeStream",
		resetStream:         "resetStream",
		sendStreamMsg:       "sendStreamMsg",
		removeStreamHandler: "removeStreamHandler",
		addStreamHandler:    "addStreamHandler",
		listeningAddrs:      "listeningAddrs",
		addPeer:             "addPeer",
		beginAdvertising:    "beginAdvertising",
		findPeer:            "findPeer",
		listPeers:           "listPeers",
	}
)

func init() {
	var v methodIdx
	if _, ok := interface{}(v).(fmt.Stringer); ok {
		_methodIdxNameToValue = map[string]methodIdx{
			interface{}(configure).(fmt.Stringer).String():           configure,
			interface{}(listen).(fmt.Stringer).String():              listen,
			interface{}(publish).(fmt.Stringer).String():             publish,
			interface{}(subscribe).(fmt.Stringer).String():           subscribe,
			interface{}(unsubscribe).(fmt.Stringer).String():         unsubscribe,
			interface{}(validationComplete).(fmt.Stringer).String():  validationComplete,
			interface{}(generateKeypair).(fmt.Stringer).String():     generateKeypair,
			interface{}(openStream).(fmt.Stringer).String():          openStream,
			interface{}(closeStream).(fmt.Stringer).String():         closeStream,
			interface{}(resetStream).(fmt.Stringer).String():         resetStream,
			interface{}(sendStreamMsg).(fmt.Stringer).String():       sendStreamMsg,
			interface{}(removeStreamHandler).(fmt.Stringer).String(): removeStreamHandler,
			interface{}(addStreamHandler).(fmt.Stringer).String():    addStreamHandler,
			interface{}(listeningAddrs).(fmt.Stringer).String():      listeningAddrs,
			interface{}(addPeer).(fmt.Stringer).String():             addPeer,
			interface{}(beginAdvertising).(fmt.Stringer).String():    beginAdvertising,
			interface{}(findPeer).(fmt.Stringer).String():            findPeer,
			interface{}(listPeers).(fmt.Stringer).String():           listPeers,
		}
	}
}

// MarshalJSON is generated so methodIdx satisfies json.Marshaler.
func (r methodIdx) MarshalJSON() ([]byte, error) {
	if s, ok := interface{}(r).(fmt.Stringer); ok {
		return json.Marshal(s.String())
	}
	s, ok := _methodIdxValueToName[r]
	if !ok {
		return nil, fmt.Errorf("invalid methodIdx: %d", r)
	}
	return json.Marshal(s)
}

// UnmarshalJSON is generated so methodIdx satisfies json.Unmarshaler.
func (r *methodIdx) UnmarshalJSON(data []byte) error {
	var s string
	if err := json.Unmarshal(data, &s); err != nil {
		return fmt.Errorf("methodIdx should be a string, got %s", data)
	}
	v, ok := _methodIdxNameToValue[s]
	if !ok {
		return fmt.Errorf("invalid methodIdx %q", s)
	}
	*r = v
	return nil
}
