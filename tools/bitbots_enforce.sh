#!/usr/bin/env bash
set -euo pipefail

UPV_JSON=".bit/persona_registry.json"
AUDIT_DIR=".bit/audit"
AUDIT_LOG="$AUDIT_DIR/bitbots_enforce.log"
mkdir -p "$AUDIT_DIR"

tmp_index="$(mktemp)"
jq -r '.[].id' "$UPV_JSON" | tr '[:upper:]' '[:lower:]' > "$tmp_index"
now=$(date -u +%FT%TZ)

for f in .bitbots/*.json; do
  [ -e "$f" ] || continue
  pid=$(jq -r '.persona_id // empty' "$f" | tr '[:upper:]' '[:lower:]')
  if ! grep -Fxq "$pid" "$tmp_index"; then
    jq --arg now "$now" '.compliance_status="fail" | .voice_enabled=false | .last_enforced_at=$now' "$f" > "$f.tmp"
    mv "$f.tmp" "$f"
    echo "$now persona_mismatch file=$f persona=$pid" >> "$AUDIT_LOG"
  else
    jq --arg now "$now" '.compliance_status="pass" | .last_enforced_at=$now' "$f" > "$f.tmp"
    mv "$f.tmp" "$f"
    echo "$now compliance_ok file=$f persona=$pid" >> "$AUDIT_LOG"
  fi
done

rm -f "$tmp_index"
chmod +x tools/bitbots_enforce.sh
