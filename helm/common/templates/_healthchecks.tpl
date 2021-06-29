{{/*
Liveness/readiness check common settings
*/}}
{{- define "healthcheck.common.settings" }}
initialDelaySeconds: {{ .healthcheck.initialDelaySeconds }}
periodSeconds: {{ .healthcheck.periodSeconds }}
failureThreshold: {{ .healthcheck.failureThreshold }}
{{- end }}

### Mina daemon healthcheck TEMPLATES ###

{{/*
Daemon startup probe settings
*/}}
{{- define "healthcheck.daemon.startupProbe" }}
startupProbe:
  tcpSocket:
    port: external-port
  failureThreshold: {{ default .Values.healthcheck.startup.failureThreshold 30 }}
  periodSeconds: {{ default .Values.healthcheck.startup.periodSeconds 10 }}
{{- end }}

{{/*
Daemon liveness check settings
*/}}
{{- define "healthcheck.daemon.livenessCheck" }}
livenessProbe:
  tcpSocket:
    port: external-port
{{- include "healthcheck.common.settings" . | indent 2 }}
{{- end }}

{{/*
Daemon readiness check settings
*/}}
{{- define "healthcheck.daemon.readinessCheck" }}
readinessProbe:
  exec:
    command: [
      "/bin/bash",
      "-c",
      "source /healthcheck/utilities.sh && isDaemonSynced && updateSyncStatusLabel {{ .name }} "
    ]
{{- include "healthcheck.common.settings" . | indent 2 }}
{{- end }}

{{/*
ALL daemon healthchecks
*/}}
{{- define "healthcheck.daemon.allChecks" }}
{{- if .healthcheck.enabled }}
{{- include "healthcheck.daemon.livenessCheck" . }}
{{- include "healthcheck.daemon.readinessCheck" . }}
{{- end }}
{{- end }}
