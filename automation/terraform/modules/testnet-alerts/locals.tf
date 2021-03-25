locals {
  cortex_image            = "grafana/cortextool:latest"
  cortextool_download_url = "https://github.com/grafana/cortex-tools/releases/download/v0.7.2/cortextool_0.7.2_linux_x86_64"
  cortextool_install_dir  = "~/.local/bin/cortextool"
}
