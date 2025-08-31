#!/usr/bin/env bash
set -euo pipefail

# Arguments:
#   $1 - IMAGE (e.g., ghcr.io/your-org/central-databank)
#   $2 - COMMIT (e.g., short git SHA)

if [ "$#" -ne 2 ]; then
  echo "Usage: $0 <IMAGE> <COMMIT>"
  exit 1
fi

IMAGE="$1"
COMMIT="$2"
STATE_DIR=".bit/state"

mkdir -p "$STATE_DIR"
TAG="${IMAGE}:${COMMIT}"

echo "$TAG" > "${STATE_DIR}/last_green.txt"
echo "[TAGGER] Last green -> $TAG"
