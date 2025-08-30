#!/usr/bin/env bash
set -euo pipefail

MANIFEST=".bit/asset_economy.yml"
SCHEMA=".bit/schema/asset_economy.schema.yml"
LOGFILE=".bithub/logs/manifest_validation.log"

echo "[INFO] Validating $MANIFEST against $SCHEMA"

# Ensure dependencies
if ! command -v yamllint >/dev/null; then
  echo "[INFO] Installing yamllint..."
  pip install --quiet yamllint
fi
if ! command -v ajv >/dev/null; then
  echo "[INFO] Installing ajv-cli..."
  npm install -g ajv-cli
fi

# YAML syntax check
yamllint "$MANIFEST" | tee "$LOGFILE"

# Schema validation (assuming JSON Schema format)
ajv validate -s "$SCHEMA" -d "$MANIFEST" --strict=true | tee -a "$LOGFILE"

echo "[SUCCESS] Manifest validation complete."
