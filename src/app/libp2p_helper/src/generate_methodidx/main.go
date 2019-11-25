// Copyright 2017 Google Inc. All rights reserved.
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to writing, software distributed
// under the License is distributed on a "AS IS" BASIS, WITHOUT WARRANTIES OR
// CONDITIONS OF ANY KIND, either express or implied.
//
// See the License for the specific language governing permissions and
// limitations under the License.

// Added as a .go file to avoid embedding issues of the template.

package main

import (
	"bytes"
	"go/format"
	"log"
	"os"
	"text/template"
)

var generatedTmpl = template.Must(template.New("generated").Parse(`
// generated by {{.Command}}; DO NOT EDIT

package {{.PackageName}}

import (
    "encoding/json"
    "fmt"
)

{{range $typename, $values := .TypesAndValues}}

var (
    _{{$typename}}NameToValue = map[string]{{$typename}} {
        {{range $values}}"{{.}}": {{.}},
        {{end}}
    }

    _{{$typename}}ValueToName = map[{{$typename}}]string {
        {{range $values}}{{.}}: "{{.}}",
        {{end}}
    }
)

func init() {
    var v {{$typename}}
    if _, ok := interface{}(v).(fmt.Stringer); ok {
        _{{$typename}}NameToValue = map[string]{{$typename}} {
            {{range $values}}interface{}({{.}}).(fmt.Stringer).String(): {{.}},
            {{end}}
        }
    }
}

// MarshalJSON is generated so {{$typename}} satisfies json.Marshaler.
func (r {{$typename}}) MarshalJSON() ([]byte, error) {
    if s, ok := interface{}(r).(fmt.Stringer); ok {
        return json.Marshal(s.String())
    }
    s, ok := _{{$typename}}ValueToName[r]
    if !ok {
        return nil, fmt.Errorf("invalid {{$typename}}: %d", r)
    }
    return json.Marshal(s)
}

// UnmarshalJSON is generated so {{$typename}} satisfies json.Unmarshaler.
func (r *{{$typename}}) UnmarshalJSON(data []byte) error {
    var s string
    if err := json.Unmarshal(data, &s); err != nil {
        return fmt.Errorf("{{$typename}} should be a string, got %s", data)
    }
    v, ok := _{{$typename}}NameToValue[s]
    if !ok {
        return fmt.Errorf("invalid {{$typename}} %q", s)
    }
    *r = v
    return nil
}

{{end}}
`))

type analysis struct {
	Command        string
	PackageName    string
	TypesAndValues map[string][]string
}

func main() {
	var buf bytes.Buffer

	an := analysis{
		Command:     "generate_methodidx",
		PackageName: "main",
		TypesAndValues: map[string][]string{
			"methodIdx": []string{"configure", "listen", "publish", "subscribe", "unsubscribe", "validationComplete", "generateKeypair", "openStream", "closeStream", "resetStream", "sendStreamMsg", "removeStreamHandler", "addStreamHandler", "listeningAddrs", "addPeer", "beginAdvertising", "findPeer"},
		},
	}

	if err := generatedTmpl.Execute(&buf, an); err != nil {
		log.Fatalf("generating code: %v", err)
	}

	src, err := format.Source(buf.Bytes())
	if err != nil {
		// Should never happen, but can arise when developing this code.
		// The user can compile the output to see the error.
		log.Printf("warning: internal error: invalid Go generated: %s", err)
		log.Printf("warning: compile the package to analyze the error")
		src = buf.Bytes()
	}
	os.Stdout.Write(src)
}
