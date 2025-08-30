#!/usr/bin/env bash
set -euo pipefail
M=".bit/patterns/universally_adaptable_ml.patterns.aln.bit"
if ! command -v yq >/dev/null; then
  sudo apt-get update && sudo apt-get install -y yq >/dev/null
fi
yq '.' "$M" >/dev/null
echo "{\"ts\":\"$(date -Iseconds)\",\"event\":\"ALN_PATTERNS_OK\",\"file\":\"$M\"}" >> .bithub/ledger/compliance.log
chmod +x .bit/loaders/lisp_verify.sh .bit/loaders/validate_adaptable_patterns.sh
