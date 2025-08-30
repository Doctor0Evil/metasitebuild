#!/usr/bin/env bash
set -euo pipefail

SOURCE="${1:-ns}"          # ns|s3|ipfs
FORCE="${2:-false}"        # true|false
LEDGER=".bithub/ledger/restore.log"
mkdir -p "$(dirname "$LEDGER")" .bithub/restore

log(){ echo "{\"ts\":\"$(date -Iseconds)\",\"event\":\"$1\",\"detail\":\"${2:-}\"}" >> "$LEDGER"; }

log "BITRESTORE_START" "source=$SOURCE"

case "$SOURCE" in
  ns)
    echo "Using local .bithub/backup and .bithub/cache if present."
    ;;
  s3)
    : "${AWS_REGION:?Missing AWS_REGION}"; : "${S3_BUCKET:?Missing S3_BUCKET}"
    PREF="${S3_PREFIX:-}"
    aws s3 sync "s3://${S3_BUCKET}/${PREF}" .bithub/restore --no-progress
    ;;
  ipfs)
    : "${IPFS_GATEWAY:?Missing IPFS_GATEWAY}"; : "${IPFS_CID:?Missing IPFS_CID}"
    wget -O .bithub/restore/payload.tar.gz "${IPFS_GATEWAY}/${IPFS_CID}"
    ;;
  *) echo "Unknown source: $SOURCE"; exit 1;;
esac

# Restore .bit
if [[ -f .bithub/backup/bit.tar.gz || -f .bithub/restore/bit.tar.gz || -f .bithub/restore/payload.tar.gz ]]; then
  SRC=$(ls -1 .bithub/backup/bit.tar.gz .bithub/restore/bit.tar.gz .bithub/restore/payload.tar.gz 2>/dev/null | head -n1)
  [[ "$FORCE" == "true" ]] && tar -xzf "$SRC" -C . || tar -xzf "$SRC" -C . --keep-old-files
  log "RESTORED_BIT" "$SRC"
fi

# Restore ledgers
if [[ -f .bithub/backup/ledger.tar.gz || -f .bithub/restore/ledger.tar.gz || -f .bithub/restore/payload.tar.gz ]]; then
  SRC=$(ls -1 .bithub/backup/ledger.tar.gz .bithub/restore/ledger.tar.gz .bithub/restore/payload.tar.gz 2>/dev/null | head -n1)
  tar -xzf "$SRC" -C .
  log "RESTORED_LEDGER" "$SRC"
fi

# Prewarm caches
for SRC in ".bithub/cache/node.tgz" ".bithub/cache/python.tgz" ".bithub/cache/dotnet.tgz" ".bithub/cache/java.tgz" \
           ".bithub/restore/node.tgz" ".bithub/restore/python.tgz" ".bithub/restore/dotnet.tgz" ".bithub/restore/java.tgz"; do
  [[ -f "$SRC" ]] || continue
  case "$SRC" in
    *node.tgz)   mkdir -p ~/.npm node_modules && tar -xzf "$SRC" -C . ;;
    *python.tgz) mkdir -p ~/.cache/pip        && tar -xzf "$SRC" -C . ;;
    *dotnet.tgz) mkdir -p ~/.nuget/packages   && tar -xzf "$SRC" -C . ;;
    *java.tgz)   mkdir -p ~/.m2/repository    && tar -xzf "$SRC" -C . ;;
  esac
  log "CACHE_PREWARMED" "$SRC"
done

log "BITRESTORE_END" "status=ok"
echo "Restore complete."

chmod +x .bit/loaders/bitrestore.sh
