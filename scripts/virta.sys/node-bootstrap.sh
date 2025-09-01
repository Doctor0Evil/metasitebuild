#!/usr/bin/env bash
set -euo pipefail

# Requires: root or sudo, ufw available, Go toolchain for builds.

log() { printf '[bootstrap] %s\n' "$*"; }

# 1) Firewall: default deny outbound, allow mirrors and federation
if command -v ufw >/dev/null 2>&1; then
  sudo ufw --force enable || true
  sudo ufw default deny outgoing
  sudo ufw default allow incoming
  # Allow HTTPS to mirrors and federation
  for host in packages.local nodes.virta time.cloudflare.com; do
    # Resolve host to IP (best-effort)
    ip=$(getent ahostsv4 "$host" | awk '{print $1; exit}' || true)
    if [[ -n "${ip:-}" ]]; then
      sudo ufw allow out to "$ip" port 443 proto tcp || true
    fi
  done
else
  log "ufw not found; please configure nftables/iptables with default-deny outbound and specific allows."
fi

# 2) Directories
sudo mkdir -p /etc/bit.hub
mkdir -p tools/ed25519 tools/gossip registries assets manifests configs/pin services/wall

# 3) Build and install proxy + gossipd
log "Building egress-proxy and gossipd..."
GO111MODULE=on go build -o ./tools/gossip/gossipd ./tools/gossip/gossipd.go
GO111MODULE=on go build -o ./services/wall/egress-proxy ./services/wall/egress-proxy.go
sudo install -m 0755 ./services/wall/egress-proxy /usr/local/bin/egress-proxy
sudo install -m 0755 ./tools/gossip/gossipd /usr/local/bin/gossipd

# 4) Environment for the wall
if [[ ! -f /etc/bit.hub/wall.env ]]; then
  cat <<'ENV' | sudo tee /etc/bit.hub/wall.env >/dev/null
# Compliance wall environment
WALL_LISTEN=:8088
# Allow only local mirrors and federation
ALLOW_DOMAINS=packages.local,nodes.virta,time.cloudflare.com,ghcr.io
# Deny big platforms explicitly
DENY_DOMAINS=github.com,api.github.com,google.com,www.google.com,*.gvt1.com,*.ggpht.com
# Headers to strip from outbound requests
REDACT_HEADERS=Authorization,X-Token,Proxy-Authorization
ENV
fi

# 5) Systemd services
log "Installing systemd units..."
sudo tee /etc/systemd/system/egress-proxy.service >/dev/null <<'UNIT'
[Unit]
Description=Bit.Hub Compliance Wall Egress Proxy
After=network-online.target
Wants=network-online.target

[Service]
EnvironmentFile=/etc/bit.hub/wall.env
ExecStart=/usr/local/bin/egress-proxy
Restart=always
RestartSec=2
AmbientCapabilities=CAP_NET_BIND_SERVICE
NoNewPrivileges=true

[Install]
WantedBy=multi-user.target
UNIT

sudo tee /etc/systemd/system/gossipd.service >/dev/null <<'UNIT'
[Unit]
Description=Bit.Hub Gossip Daemon
After=network-online.target
Wants=network-online.target

[Service]
ExecStart=/usr/local/bin/gossipd -listen :7777 -peers nodes.virta:7777 -topics ledger,asset-index
Restart=always
RestartSec=2
NoNewPrivileges=true

[Install]
WantedBy=multi-user.target
UNIT

# Optional: watchdog if you have a Lisp runtime on Linux
if command -v sbcl >/dev/null 2>&1; then
  sudo tee /etc/systemd/system/wall-watchdog.service >/dev/null <<'UNIT'
[Unit]
Description=Bit.Hub Compliance Wall Watchdog
After=egress-proxy.service

[Service]
WorkingDirectory=/opt/bit.hub
ExecStart=/usr/bin/sbcl --script services/wall/watchdog.lisp
Restart=always
RestartSec=5
NoNewPrivileges=true

[Install]
WantedBy=multi-user.target
UNIT
fi

# 6) Enable and start services
sudo systemctl daemon-reload
sudo systemctl enable --now egress-proxy.service
sudo systemctl enable --now gossipd.service
if systemctl list-unit-files | grep -q wall-watchdog.service; then
  sudo systemctl enable --now wall-watchdog.service || true
fi

# 7) Mirror pinned actions/toolchains via proxy
if [[ -f scripts/actions-mirror.sh ]]; then
  log "Mirroring pinned actions via proxy..."
  bash scripts/actions-mirror.sh
else
  log "scripts/actions-mirror.sh not found; skipping action mirror."
fi

log "virta.sys Linux bootstrap complete."
