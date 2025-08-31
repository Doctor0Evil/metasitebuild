#!/usr/bin/env bash
set -euo pipefail

BIT_HUB_REPO_URL="${BIT_HUB_REPO_URL:-https://github.com/Doctor0Evil/Bit.Hub.git}"
CACHE_DIR="${CACHE_DIR:-/opt/bitbot/cache}"
LABELS="${RUNNER_LABELS:-self-hosted,bit.hub,linux,pattern.sentient}"
mkdir -p "$CACHE_DIR"

echo "[bitbot] Bootstrapping runner…"
if git ls-remote "$BIT_HUB_REPO_URL" >/dev/null 2>&1; then
  rm -rf "$CACHE_DIR/Bit.Hub" && git clone --depth=1 "$BIT_HUB_REPO_URL" "$CACHE_DIR/Bit.Hub"
  cp -r "$CACHE_DIR/Bit.Hub/.bithub/policy" "$CACHE_DIR/policy" || true
  cp -r "$CACHE_DIR/Bit.Hub/.bit/patterns" "$CACHE_DIR/patterns" || true
  cp -r "$CACHE_DIR/Bit.Hub/.bit/schemas" "$CACHE_DIR/schemas" || true
fi

echo "$LABELS" > "$CACHE_DIR/labels.txt"
echo "[bitbot] Runner labels: $LABELS"
echo "[bitbot] Bootstrap complete."
exit 0
