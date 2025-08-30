#!/usr/bin/env bash
set -euo pipefail

CMD="${1:-help}"

# Resolve token (BITHUB over GitHub fallback)
RESOLVE_TOKEN() {
  if [[ -n "${BITHUB_TOKEN:-}" ]]; then echo "$BITHUB_TOKEN"; elif [[ -n "${GITHUB_TOKEN:-}" ]]; then echo "$GITHUB_TOKEN"; else echo ""; fi
}

ensure_tools() {
  for t in gh jq yq git; do
    command -v "$t" >/dev/null 2>&1 || { echo "Missing tool: $t"; exit 1; }
  done
}

auth_gh() {
  local tok; tok="$(RESOLVE_TOKEN)"
  [[ -z "$tok" ]] && { echo "No token found (BITHUB_TOKEN/GITHUB_TOKEN)"; exit 1; }
  echo "$tok" | gh auth login --with-token >/dev/null 2>&1 || true
}

plan() {
  echo "[.bithub] Planning orchestration…"
  if [[ -f ".bithub.yml" ]]; then
    yq '.orchestration.flows[0]' .bithub.yml
  else
    echo "No .bithub.yml found; using synthesized defaults."
  fi
}

ci_run() {
  echo "[.bithub] Kicking orchestrator workflow…"
  auth_gh
  local ref; ref="${GITHUB_REF_NAME:-$(git rev-parse --abbrev-ref HEAD)}"
  gh workflow run ".bithub-actions Orchestration" --ref "$ref" || {
    echo "Fallback: directly call workflow file name"
    gh workflow run ".bithub-actions.yml" --ref "$ref" || true
  }
  gh run watch --exit-status || true
}

sync() {
  echo "[.bithub] Sync to nodes…"
  ./.bithub/overhaul/sync.to.nodes.sh
}

status() {
  echo "[.bithub] Status"
  echo "- Repo: ${GITHUB_REPOSITORY:-local}"
  echo "- Ref:  ${GITHUB_REF_NAME:-$(git rev-parse --abbrev-ref HEAD)}"
  echo "- Token: $( [[ -n "${BITHUB_TOKEN:-}" ]] && echo 'BITHUB_TOKEN' || ([[ -n "${GITHUB_TOKEN:-}" ]] && echo 'GITHUB_TOKEN' || echo 'none') )"
}

help() {
  cat <<EOF
.bithub .git.command-interface

Usage:
  .bithub/overhaul/.git.command-interface.sh <command>

Commands:
  plan     Show orchestration flow (from .bithub.yml)
  ci       Run orchestrator workflow (gh CLI)
  sync     Sync .bithub *.json and *.bit files to nodes
  status   Show environment + token resolution
  help     This message
EOF
}

ensure_tools

case "$CMD" in
  plan) plan ;;
  ci) ci_run ;;
  sync) sync ;;
  status) status ;;
  help|*) help ;;
esac
