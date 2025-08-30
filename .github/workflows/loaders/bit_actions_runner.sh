#!/usr/bin/env bash
set -euo pipefail

PIPE=".bit-actions/pipelines/site.build.aln.yml"
REG=".bit-actions/bit.actions.registry.aln.yml"
LEDGER=".bithub/ledger/compliance.log"

reqbin() { command -v "$1" >/dev/null || { echo "Missing $1"; exit 1; }; }
reqbin yq jq

log() { echo "{\"ts\":\"$(date -Iseconds)\",\"event\":\"$1\",\"detail\":\"${2:-}\"}" >> "$LEDGER"; }

# Toolchains
setup_toolchain() {
  local t="$1"
  case "$t" in
    dotnet) echo "::group::setup-dotnet"; echo "dotnet via GH action"; echo "::endgroup::" ;;
    java)   echo "::group::setup-java";   echo "java via GH action";   echo "::endgroup::" ;;
    node)   echo "::group::setup-node";   echo "node via GH action";   echo "::endgroup::" ;;
    python) echo "::group::setup-python"; echo "python via GH action"; echo "::endgroup::" ;;
  esac
}

# Parse manifest
TOOLCHAINS=$(yq -r '.pipeline.setup.toolchains[]?' "$PIPE" || true)
CACHES=$(yq -r '.pipeline.setup.caches[]?' "$PIPE" || true)

log "BIT_ACTIONS_START" "pipeline=$(yq -r '.pipeline.id' "$PIPE")"

# Run steps
COUNT=$(yq '.pipeline.steps | length' "$PIPE")
for ((i=0; i<COUNT; i++)); do
  SID=$(yq -r ".pipeline.steps[$i].id" "$PIPE")
  log "STEP_START" "$SID"

  # Conditional execution
  if yq -e ".pipeline.steps[$i].if_equals" "$PIPE" >/dev/null 2>&1; then
    REF_STEP=$(yq -r ".pipeline.steps[$i].if_equals.step" "$PIPE")
    WANT=$(yq -r ".pipeline.steps[$i].if_equals.value" "$PIPE")
    GOT=$(jq -r '.'"$(yq -r ".pipeline.steps[] | select(.id==\"$REF_STEP\").parse.select" "$PIPE")" site_status.json 2>/dev/null || echo "")
    [[ "$GOT" == "$WANT" ]] || { log "STEP_SKIP" "$SID"; continue; }
  fi

  # Parse JSON selector
  if yq -e ".pipeline.steps[$i].parse" "$PIPE" >/dev/null 2>&1; then
    SRC=$(yq -r ".pipeline.steps[$i].parse.from_json" "$PIPE")
    SEL=$(yq -r ".pipeline.steps[$i].parse.select" "$PIPE")
    VAL=$(jq -r "$SEL" "$SRC")
    echo "$VAL" > ".bithub/reports/${SID}.val"
  fi

  # Run shell
  if yq -e ".pipeline.steps[$i].run" "$PIPE" >/dev/null 2>&1; then
    bash -eo pipefail -c "$(yq -r ".pipeline.steps[$i].run" "$PIPE")"
  fi

  # Upload instruction is handled by GH workflow step (not here)

  log "STEP_END" "$SID"
done

log "BIT_ACTIONS_END"
