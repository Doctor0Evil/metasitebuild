#!/usr/bin/env bash
set -euo pipefail

SHEET=".bit-actions/compliance.bit.aln.yml"
[[ -f "$SHEET" ]] || { echo "Missing $SHEET"; exit 1; }

req() { command -v "$1" >/dev/null || { sudo apt-get update && sudo apt-get install -y "$1"; }; }
req yq; req jq

SAFE_JSON=$(yq -o=json '.compliance.actions.safelist' "$SHEET")
SAFE_RE=$(echo "$SAFE_JSON" | jq -r 'join("|")')
FALLBACK=$(yq -r '.compliance.actions.fallback_safe_action' "$SHEET")

# Build replacement map
TMPMAP=$(mktemp)
{
  yq -o=json '.compliance.actions.replace_map' "$SHEET"
} > "$TMPMAP"

# Wildcards
WILDC_JSON=$(yq -o=json '.compliance.actions.wildcard_map' "$SHEET")

for wf in $(find .github/workflows -type f -name "*.yml" -o -name "*.yaml"); do
  echo "Patching $wf"

  # permissions
  yq -e '.permissions' "$wf" >/dev/null 2>&1 || yq -i '.permissions = {"contents":"read"}' "$wf"

  # compliance job
  yq -e '.jobs.compliance' "$wf" >/dev/null 2>&1 || yq -i '.jobs.compliance = {"runs-on":"ubuntu-latest","steps":[{"uses":"./.github/actions/bithub-compliance-gate"}]}' "$wf"

  # needs: compliance
  for j in $(yq -r '.jobs | keys[]' "$wf"); do
    [[ "$j" == "compliance" ]] && continue
    if ! yq -e ".jobs[\"$j\"].needs" "$wf" >/dev/null 2>&1; then
      yq -i ".jobs[\"$j\"].needs = \"compliance\"" "$wf"
    else
      yq -e ".jobs[\"$j\"].needs | (.. | scalars) | select(.==\"compliance\")" "$wf" >/dev/null 2>&1 || yq -i ".jobs[\"$j\"].needs += [\"compliance\"]" "$wf"
    fi
  done

  # swap disallowed actions
  while read -r USES; do
    [[ -z "$USES" ]] && continue
    if ! [[ "$USES" =~ $SAFE_RE ]]; then
      # exact
      REPL=$(jq -r --arg k "$USES" '.[$k] // empty' "$TMPMAP")
      # wildcard
      if [[ -z "$REPL" ]]; then
        COUNT=$(echo "$WILDC_JSON" | jq 'length')
        for ((i=0;i<COUNT;i++)); do
          FROM=$(echo "$WILDC_JSON" | jq -r ".[$i].from")
          TO=$(echo "$WILDC_JSON" | jq -r ".[$i].to")
          BASE="${USES%@*}@*"
          if [[ "$BASE" == "$FROM" ]]; then REPL="$TO"; break; fi
        done
      fi
      # fallback
      [[ -z "$REPL" ]] && REPL="$FALLBACK"
      echo "  - Replacing $USES -> $REPL"
      # precise replacement of the 'uses:' value
      yq -i "(.jobs[]?.steps[]? | select(has(\"uses\") and .uses == \"$USES\") | .uses) = \"$REPL\"" "$wf"
    fi
  done < <(yq -r '.jobs[]?.steps[]? | select(has("uses")) | .uses' "$wf")

  # block run pipes by commenting them
  RX=$(yq -r '.compliance.forbid.run_pipes_regex' "$SHEET")
  if grep -Eq "$RX" "$wf"; then
    perl -0777 -pe 's/(run:\s*)(.*(curl|wget).*?\|.*bash.*)/$1"# Blocked by Bit.Hub"\n\1echo "Replace with a safelisted action or loader"/igs' -i "$wf"
  fi
done

echo "Auto-patch complete."
