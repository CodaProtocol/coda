### Mina SNARK-coordinator/worker healthcheck TEMPLATES ###

{{/*
snark-coordinator startup probe settings
*/}}
{{- define "healthcheck.snarkCoordinator.startupProbe" }}
{{- include "healthcheck.daemon.startupProbe" . }}
{{- end }}

{{/*
snark-coordinator liveness settings
*/}}
{{- define "healthcheck.snarkCoordinator.livenessCheck" }}
{{- include "healthcheck.daemon.livenessCheck" . }}
{{- end }}

{{/*
snark-coordinator readiness settings
*/}}
{{- define "healthcheck.snarkCoordinator.readinessCheck" }}
readinessProbe:
  exec:
    command: [
      "/bin/bash",
      "-c",
      "source /healthcheck/utilities.sh && isDaemonSynced && hasSnarkWorker && updateSyncStatusLabel {{ .name }}"
    ]
{{- end }}

{{/*
ALL snark-coordinator healthchecks - TODO: readd startupProbes once GKE clusters have been updated to 1.16
*/}}
{{- define "healthcheck.snarkCoordinator.allChecks" }}
{{- if .healthcheck.enabled }}
{{- include "healthcheck.snarkCoordinator.livenessCheck" . }}
{{- include "healthcheck.snarkCoordinator.readinessCheck" . }}
{{- end }}
{{- end }}
