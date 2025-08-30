#!/usr/bin/env bash
set -euo pipefail

SEC="SECURITY.md"
[[ -f "$SEC" ]] || { echo "No SECURITY.md found"; exit 0; }

# Extract version support table (simple parser for GitHub-flavored table)
table=$(awk '/\|[[:space:]]*Version[[:space:]]*\|/{flag=1;next}/^$/{flag=0}flag' "$SEC" || true)
[[ -z "$table" ]] && { echo "No version table detected"; exit 0; }

# Build JSON: [{version:"5.1.x", supported:true}, ...]
json="[]"
while IFS= read -r line; do
  [[ "$line" =~ ^\| ]] || continue
  ver=$(awk -F'|' '{gsub(/ /,""); print $2}' <<<"$line")
  sup=$(awk -F'|' '{gsub(/ /,""); print $3}' <<<"$line")
  [[ "$ver" == "Version" || "$ver" == "-------" ]] && continue
  case "$sup" in
    ":white_check_mark:") supported=true ;;
    ":x:") supported=false ;;
    *) continue ;;
  esac
  tmp=$(mktemp)
  jq --arg v "$ver" --argjson s "$supported" '. + [ {version:$v, supported:$s} ]' <<<"$json" > "$tmp" && json="$(cat "$tmp")"
done <<< "$table"

mkdir -p orchestration_audit
echo "$json" > orchestration_audit/supported_versions.json
echo "[enforce] Extracted supported versions → orchestration_audit/supported_versions.json"
