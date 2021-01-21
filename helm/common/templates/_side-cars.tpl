### Mina Testnet side-car container TEMPLATES ###

# *** Watchman specifications ***

{{/*
Side-Car - Watchman: volume definition
*/}}
{{- define "sideCar.watchman.volume" }}
{{- if .watchman.enable }}
- name: {{ .watchman.volumeName }}
  emptyDir: {}
{{- end }}
{{- end }}

{{/*
Side-Car - Watchman: volume-mount definition
*/}}
{{- define "sideCar.watchman.volumeMount" }}
{{- if .watchman.enable }}
- name: {{ .watchman.volumeName }}
  mountPath: {{ .watchman.volumeMountPath }}
{{- end }}
{{- end }}

{{/*
Side-Car - Watchman: environment variable definition
*/}}
{{- define "sideCar.watchman.envVars" }}
- name: WATCHMAN_MOUNT_PATH
  value: {{ .watchman.volumeMountPath }}
{{- range $key, $val := .watchman.envVars }}
- name: {{ $key }}
  value: {{ $val | quote }}
{{- end }}
{{- end }}

{{/*
Side-Car - Watchman: container definition
*/}}
{{- define "sideCar.watchman.containerSpec" }}
{{- if .watchman.enable }}
- name: watchman
  image: {{ .watchman.image }}
  args: {{ .watchman.command }}
  imagePullPolicy: Always
  volumeMounts:
{{- include "sideCar.watchman.volumeMount" . | indent 2 }}
  env:
{{- include "sideCar.watchman.envVars" . | indent 2 }}
{{- end }}
{{- end }}

# *** Core Dump (Watch) Helpers ***

{{/*
Side-Car - Watchman: container life-cycle definition for post-start/pre-stop operations related to collecting Core dumps
*/}}
{{- define "sideCar.watchman.lifeCycleHook.coreDump" }}
{{- if .watchman.enable }}
lifecycle:
  postStart:
    exec:
      command: ["echo", "'{{ .watchman.volumeMountPath }}/core.%h.%e.%t' > /proc/sys/kernel/core_pattern"]
  preStop:
    exec:
      command: ["ls","-lah", "{{ .watchman.volumeMountPath }}"]
{{- end }}
{{- end }}
