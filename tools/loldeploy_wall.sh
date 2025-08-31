#!/usr/bin/env bash
set -Eeuo pipefail
IFS=$'\n\t'

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
ARTI_DIR="${REPO_ROOT}/artifacts"
STATE_DIR="${REPO_ROOT}/.bit/state"
MANIFEST="${REPO_ROOT}/.loldeploy.cont"
LOCK="${REPO_ROOT}/.bit/ethics-guard.lock"
SPEC="${REPO_ROOT}/.bit/environment.ethics.yml"
GUARD="${REPO_ROOT}/tools/ethics-guard.sh"

mkdir -p "${ARTI_DIR}" "${STATE_DIR}"

need() { command -v "$1" >/dev/null || { echo "Missing $1" >&2; exit 2; }; }
need awk; need sed; need git; need sha256sum || true

# Read a value from .loldeploy.cont (simple parser for demo)
get_val() { awk -v K="$1" '
  BEGIN{FS=":"}
  /^[[:space:]]*[#;]/ {next}
  { line=$0 }
  $1 ~ K {
    sub(/^[[:space:]]*/,"",$2);
    gsub(/[",]/,"",$2);
    print $2
  }' "${MANIFEST}" | head -n1; }

# Hash helpers
h256() { command -v sha256sum >/dev/null && sha256sum "$1" | awk '{print $1}' || shasum -a 256 "$1" | awk '{print $1}'; }

# Verify lock integrity
want_script=$(awk -F= '/^script_sha256=/{print $2}' "$LOCK")
want_spec=$(awk -F= '/^spec_sha256=/{print $2}' "$LOCK")
got_script=$(h256 "$GUARD")
got_spec=$(h256 "$SPEC")
[[ "$want_script" == "$got_script" ]] || { echo "Guard integrity mismatch"; exit 2; }
[[ "$want_spec"  == "$got_spec"  ]] || { echo "Spec  integrity mismatch"; exit 2; }

# Run ethics-guard (produces JSON + ALN)
bash "$GUARD" "$SPEC" strict "${REPO_ROOT}/compliance_report.json" "${REPO_ROOT}/compliance_report.aln" || true

STATUS=$(jq -r .status "${REPO_ROOT}/compliance_report.json" 2>/dev/null || echo "unknown")
REQUIRED=$(awk -v K="status_required" '
  BEGIN{FS=":"}
  $1 ~ K { v=$2; gsub(/[", ]/,"",v); print v }' "${MANIFEST}")
ACTION=$(awk -v K="action" '
  BEGIN{FS=":"}
  $1 ~ K { v=$2; gsub(/[", ]/,"",v); print v }' "${MANIFEST}")

# Discover current image tag (last commit, or derive from env)
IMAGE=$(awk -v K="image" '
  BEGIN{FS=":"}
  $1 ~ K { v=$2; gsub(/[", ]/,"",v); print v }' "${MANIFEST}")
COMMIT=$(git -C "${REPO_ROOT}" rev-parse --short HEAD)
TAG="${IMAGE}:${COMMIT}"

LAST_GREEN="${STATE_DIR}/last_green.txt"

record_green() {
  echo "$TAG" > "$LAST_GREEN"
  cp -f "${REPO_ROOT}/compliance_report.json" "${STATE_DIR}/last_green_report.json" || true
  cp -f "${REPO_ROOT}/compliance_report.aln"  "${STATE_DIR}/last_green_report.aln"  || true
  echo "[WALL] Recorded green release: $TAG"
}

redeploy_last_green() {
  if [[ ! -f "$LAST_GREEN" ]]; then
    echo "[WALL] No green release recorded; cannot redeploy." >&2
    exit 3
  fi
  GREEN_TAG=$(cat "$LAST_GREEN")
  echo "[WALL] Breach detected. Redeploying last green: $GREEN_TAG"
  # Example K8s rollout (replace with your Helm/kubectl)
  kubectl -n aln set image deploy/central-databank central-databank="${GREEN_TAG}"
  kubectl -n aln rollout status deploy/central-databank --timeout=180s
}

# Decide pass/fail
should_pass=false
case "$REQUIRED" in
  pass) [[ "$STATUS" == "pass" ]] && should_pass=true ;;
  warn) [[ "$STATUS" == "pass" || "$STATUS" == "warn" ]] && should_pass=true ;;
  any)  should_pass=true ;;
  *)    [[ "$STATUS" == "pass" ]] && should_pass=true ;;
esac

if $should_pass; then
  record_green
else
  echo "[WALL] Compliance status=$STATUS required=$REQUIRED => breach"
  if [[ "$ACTION" == "redeploy_last_green" ]]; then
    redeploy_last_green
  else
    echo "[WALL] Freeze policy active; exiting non-zero."
    exit 4
  fi
fi

# Self-replication (push guard/spec/manifest to mirrors)
mirror_urls=$(awk '
  /mirrors:/,/\]/ {print}
' "${MANIFEST}" | sed -n 's/.*url:[[:space:]]*"\(.*\)".*/\1/p')

for url in $mirror_urls; do
  echo "[WALL] Replicating policy files to mirror: $url"
  tmp=$(mktemp -d)
  git clone --depth 1 "$url" "$tmp"
  cp -f "$MANIFEST" "$tmp/.loldeploy.cont"
  mkdir -p "$tmp/.bit" "$tmp/tools"
  cp -f "$LOCK" "$tmp/.bit/ethics-guard.lock"
  cp -f "$SPEC" "$tmp/.bit/environment.ethics.yml"
  cp -f "$GUARD" "$tmp/tools/ethics-guard.sh"
  pushd "$tmp" >/dev/null
    git config user.name  "BitHub-ComplianceWall"
    git config user.email "compliance.wall@local"
    git add . && git commit -m "Sync compliance wall (auto)" || true
    git push origin HEAD || echo "[WALL] Mirror push failed (logged)"
  popd >/dev/null
  rm -rf "$tmp"
done

echo "[WALL] Completed."
chmod +x tools/loldeploy_wall.sh
