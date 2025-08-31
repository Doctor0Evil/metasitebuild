#!/usr/bin/env bash
set -euo pipefail

echo "[Bit.Hub] Compliance Block starting..."

# Load terms
TERMS_FILE=".bit/terms.compliance.yield.json"
if [ ! -f "$TERMS_FILE" ]; then
  echo "::error::Compliance terms file missing."
  exit 1
fi

# Check runner labels
if [[ "${RUNNER_LABELS:-}" != *"bit.hub"* ]]; then
  echo "::error::Runner not labeled for Bit.Hub compliance."
  exit 1
fi

# Check workflow file for required defaults
for wf in $(find .github/workflows -type f -name '*.yml' -o -name '*.yaml'); do
  if ! grep -q "permissions:" "$wf"; then
    echo "::error::Workflow $wf missing permissions block."
    exit 1
  fi
done

echo "[Bit.Hub] Compliance Block passed."
