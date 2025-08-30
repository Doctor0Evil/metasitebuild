#!/usr/bin/env bash
set -euo pipefail

CFG=".bithub/overhaul/sync.to.nodes.yml"
[[ -f "$CFG" ]] || { echo "Missing $CFG"; exit 1; }

command -v yq >/dev/null || { echo "yq not found"; exit 1; }
command -v jq >/dev/null || { echo "jq not found"; exit 1; }

attempts="$(yq -r '.retries.attempts // 3' "$CFG")"
backoff="$(yq -r '.retries.backoffSeconds // 5' "$CFG")"

# Collect files matched by accept globs
collect_files() {
  local globs_json="$1"
  tmp=$(mktemp)
  echo "[]" > "$tmp"
  echo "$globs_json" | jq -r '.[]' | while read -r pattern; do
    while IFS= read -r -d '' file; do
      jf=$(mktemp)
      jq --arg f "$file" '. + [ $f ]' "$tmp" > "$jf" && mv "$jf" "$tmp"
    done < <(bash -lc "shopt -s globstar nullglob; printf '%s\0' $pattern")
  done
  cat "$tmp" | jq -c 'unique'
}

send_payload() {
  local name="$1" api="$2" header="$3" token="$4" files_json="$5"
  echo "[sync] → $name ($api)"
  body=$(jq -c -n --arg repo "${GITHUB_REPOSITORY:-local}" \
                 --arg sha  "${GITHUB_SHA:-unknown}" \
                 --arg ts   "$(date -Iseconds)" \
                 --argjson files "$files_json" \
                 '{repo:$repo, sha:$sha, ts:$ts, files:$files}')
  n=0
  until curl -sSf -X POST "$api" -H "$header: Bearer $token" -H "Content-Type: application/json" -d "$body"; do
    n=$((n+1))
    [[ $n -ge $attempts ]] && { echo "[sync] failed: $name"; return 1; }
    sleep $(( backoff * 2**(n-1) ))
  done
  echo "[sync] ok: $name"
}

nodes_count=$(yq -r '.nodes | length' "$CFG")
[[ "$nodes_count" -gt 0 ]] || { echo "No nodes configured"; exit 0; }

for i in $(seq 0 $((nodes_count-1))); do
  name=$(yq -r ".nodes[$i].name" "$CFG")
  api=$(yq -r ".nodes[$i].api" "$CFG")
  header=$(yq -r ".nodes[$i].auth.header // \"Authorization\"" "$CFG")
  tokenRef=$(yq -r ".nodes[$i].auth.tokenRef" "$CFG")
  accept=$(yq -r ".nodes[$i].accept" "$CFG" | jq -c '.')

  # Resolve token from tokenRef (env:VAR)
  if [[ "$tokenRef" == env:* ]]; then
    var="${tokenRef#env:}"
    token="${!var:-}"
  else
    token=""
  fi
  [[ -z "$token" ]] && { echo "[sync] Missing token for $name"; continue; }

  files_json="$(collect_files "$accept")"
  if [[ "$(echo "$files_json" | jq 'length')" -eq 0 ]]; then
    echo "[sync] No files to send for $name"
    continue
  fi

  send_payload "$name" "$api" "$header" "$token" "$files_json" || true
done
