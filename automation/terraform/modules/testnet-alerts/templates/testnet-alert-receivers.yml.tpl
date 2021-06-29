# $: pagerduty_service_key - service integration key to trigger PagerDuty pages
# $: pagerduty_alert_filter - regular expression indicating set of testnets to monitor/alert via PagerDuty
# $: slack_alert_webhook - Slack integration webhook for sending mainnet and devnet warnings and for all non-(mainnet|devnet) alerting

global: {}
receivers:
  - name: pagerduty-testnet-primary
    pagerduty_configs:
      - service_key: ${pagerduty_service_key}
  - name: slack-alert-default
    slack_configs:
      - api_url: ${slack_alert_webhook}
        channel: '#testnet-warnings'
        send_resolved: true
        text: >-
          {{ if eq .Status "firing" }}All active alerts:{{ end }}

          {{ range .Alerts -}}
            *Alert:* {{ .Labels.alertname }}{{ if .Labels.severity }} - `{{ .Labels.severity }}`{{ end }}
            *Description:* {{ .Annotations.description }}
            *Runbook:* {{ if .Annotations.runbook }} `{{ .Annotations.runbook }}` {{ end }}

          {{ end }}

route:
  receiver: slack-alert-default
  group_by:
    - testnet
  routes:
    - receiver: pagerduty-testnet-primary
      match_re:
        testnet: ^(${pagerduty_alert_filter})$
        severity: critical
    - receiver: slack-alert-default
      match_re:
        testnet: ^(${pagerduty_alert_filter})$
        severity: warning
