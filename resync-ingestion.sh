#!/usr/bin/env bash
set -euo pipefail

echo "🔄 Starting ingestion resync at $(date -u +%FT%TZ)"

# --- Config ---
LEDGER_FILE="registries/command-ledger.alnlog"
MANIFEST_FILE="manifests/master-enforcement.aln"
INGEST_DIR="data/ingestion"

# --- Verify environment ---
if [[ ! -f "$MANIFEST_FILE" ]]; then
  echo "❌ ERROR: Enforcement manifest not found at $MANIFEST_FILE"
  exit 1
fi

# --- Optional: verify manifest signature ---
if command -v verify-ed25519 >/dev/null 2>&1; then
  if ! verify-ed25519 "$MANIFEST_FILE" ".keys/ed25519.public"; then
    echo "❌ ERROR: Manifest signature invalid"
    exit 1
  fi
else
  echo "⚠️  verify-ed25519 not installed; skipping signature check"
fi

# --- Prepare ingestion directory ---
mkdir -p "$INGEST_DIR"

# --- Fetch or refresh ingestion sources ---
# Replace these with your actual ingestion source sync commands
echo "📥 Syncing ingestion sources..."
if [[ -f scripts/fetch-ingestion-sources.sh ]]; then
  bash scripts/fetch-ingestion-sources.sh
else
  echo "⚠️  No fetch-ingestion-sources.sh found; skipping source fetch"
fi

# --- Run ingestion pipeline ---
echo "🚀 Running ingestion pipeline..."
if [[ -f scripts/run-ingestion-pipeline.sh ]]; then
  bash scripts/run-ingestion-pipeline.sh "$INGEST_DIR"
else
  echo "⚠️  No run-ingestion-pipeline.sh found; skipping pipeline run"
fi

# --- Append audit entry to ledger ---
HASH=$(sha256sum "$MANIFEST_FILE" | awk '{print $1}')
echo "{\"ts\":\"$(date -u +%FT%TZ)\",\"bot\":\"resync-ingestion\",\"repo\":\"${GITHUB_REPOSITORY:-local}\",\"payload\":\"$HASH\"}" >> "$LEDGER_FILE"

echo "✅ Ingestion resync complete."
