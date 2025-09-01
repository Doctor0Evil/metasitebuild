#!/usr/bin/env bash
set -euo pipefail

PINS="configs/pin/gha-pins.aln"
CACHE="tools/actions/cache"
PROXY="http://127.0.0.1:8088/fetch?url="

mkdir -p "$CACHE"

if [[ ! -f "$PINS" ]]; then
  echo "Missing $PINS"
  exit 1
fi

while IFS= read -r line; do
  [[ -z "$line" ]] && continue
  [[ "$line" =~ ^# ]] && continue
  # Expected: owner/repo@sha256:... <url>
  ref=$(echo "$line" | awk '{print $1}')
  url=$(echo "$line" | awk '{print $2}')
  owner_repo=${ref%%@*}
  sha=${ref##*@}
  dest="$CACHE/${owner_repo//\//_}_$(echo "$sha" | tr ':' '_').zip"

  if [[ -f "$dest" ]]; then
    echo "Cached $ref"
    continue
  fi

  enc=$(python3 - <<PY
import urllib.parse,sys
print(urllib.parse.quote(sys.argv[1], safe=''))
PY
"$url")
  echo "Mirroring $ref"
  curl -fsSL "${PROXY}${enc}" -o "$dest"
done < "$PINS"
