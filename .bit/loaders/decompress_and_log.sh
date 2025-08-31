#!/usr/bin/env bash
# Args: $1 = input path, $2 = output path, $3 = method
set -euo pipefail
METHOD=${3:-"gzip"}
LOG=".bithub/ledger/runner.log"
case "$METHOD" in
  "gzip") gzip -d "$1" -c > "$2";;
  "tar") tar -xzvf "$1" -C "$2";;
  # ... other methods ...
  *) echo "[ERROR] unknown method $METHOD"; exit 1;;
esac
echo "{\"ts\":\"$(date -Iseconds)\",\"event\":\"DECOMPRESS\",\"src\":\"$1\",\"dst\":\"$2\",\"method\":\"$METHOD\"}" >> "$LOG"
