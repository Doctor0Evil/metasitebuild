#!/usr/bin/env bash
set -euo pipefail
CONTINUITY="${CONTINUITY_PATH:-.bit/continuity.json}"
POLICY_DIR="${POLICY_DIR:-.bithub/policy}"
CACHE_DIR="${CACHE_DIR:-/var/cache/bitbot}"
BEACON_URL="${BEACON_URL:-}"
mkdir -p "$CACHE_DIR"

ts() { date -u +'%Y-%m-%dT%H:%M:%SZ'; }

echo "[$(ts)] BitBot heartbeat starting..."
if [ -f "$CONTINUITY" ]; then
  cp "$CONTINUITY" "$CACHE_DIR/continuity.json"
fi

if [ -n "${BEACON_URL}" ]; then
  payload=$(jq -n \
    --arg host "$(hostname)" \
    --arg os "$(uname -sr)" \
    --arg labels "${RUNNER_LABELS:-bit.hub,vm}" \
    --arg ts "$(ts)" \
    '{event:"runner_heartbeat", host:$host, os:$os, labels:$labels, timestamp:$ts}')
  curl -fsS "$BEACON_URL" -H "Content-Type: application/json" -d "$payload" || true
fi

echo "[$(ts)] Heartbeat done."
