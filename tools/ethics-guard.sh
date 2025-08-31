#!/usr/bin/env bash
# Bit.Hub / ALN Ethics Guard
# Purpose: Validate environment ethics spec, emit JSON + ALN evidence, block bypass
# Shell hardening
set -Eeuo pipefail
IFS=$'\n\t'

# Minimal, controlled PATH
export PATH="/usr/sbin:/usr/bin:/sbin:/bin"
umask 027

# -------- Config --------
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
WORKDIR="${REPO_ROOT}"
LOCKFILE="${REPO_ROOT}/.bit/ethics-guard.lock"
SPEC_PATH="${1:-${REPO_ROOT}/.bit/environment.ethics.yml}"
ALN_MODE="${2:-strict}"          # strict | permissive
OUT_JSON="${3:-${REPO_ROOT}/compliance_report.json}"
OUT_ALN="${4:-${REPO_ROOT}/compliance_report.aln}"
ARTIFACTS_DIR="${REPO_ROOT}/artifacts"
mkdir -p "${ARTIFACTS_DIR}"

# -------- Env allowlist --------
# Drop untrusted env except a short allowlist (avoid injection/bypass)
preserve_envs=(HOME USER LOGNAME SHELL PATH RUNNER_NAME RUNNER_OS GITHUB_ACTIONS GITHUB_RUN_ID GITHUB_SHA GITHUB_REF BIT_HUB_NODE_ID ALN_COMPLIANCE_MODE ALN_FAILSAFE)
tmp_env="$(mktemp)"
export -p >"$tmp_env"
# shellcheck disable=SC2162
while read line; do
  case "$line" in
    declare\ -x\ *=*) var="${line#declare -x }"; var="${var%%=*}"
      keep=0
      for k in "${preserve_envs[@]}"; do [[ "$var" == "$k" ]] && keep=1 && break; done
      [[ $keep -eq 0 ]] && unset "$var" || true
    ;;
  esac
done <"$tmp_env"
rm -f "$tmp_env"

# -------- Helpers --------
die() { echo "[ETHICS-GUARD] ERROR: $*" >&2; exit 2; }
warn(){ echo "[ETHICS-GUARD] WARN: $*" >&2; }
info(){ echo "[ETHICS-GUARD] INFO: $*"; }

sha256() { command -v sha256sum >/dev/null && sha256sum "$1" | awk '{print $1}' || (command -v shasum >/dev/null && shasum -a 256 "$1" | awk '{print $1}'); }
now_iso() { date -u +"%Y-%m-%dT%H:%M:%SZ"; }
hostname_safe() { hostname 2>/dev/null || echo "unknown-host"; }

git_commit() {
  (cd "${REPO_ROOT}" && git rev-parse --verify HEAD 2>/dev/null) || echo "unknown"
}

require_cmd() {
  command -v "$1" >/dev/null || die "Missing required command: $1"
}

# -------- Integrity lock (anti-bypass) --------
# The lockfile pins the expected SHA256 of the spec and this script.
# Any change requires an explicit, reviewed lock update.
if [[ ! -f "${LOCKFILE}" ]]; then
  die "Missing lockfile at ${LOCKFILE}. Create it with expected hashes to enable guard."
fi

EXPECTED_SCRIPT_SHA="$(awk -F= '/^script_sha256=/{print $2}' "${LOCKFILE}")"
EXPECTED_SPEC_SHA="$(awk -F= '/^spec_sha256=/{print $2}' "${LOCKFILE}")"
ENFORCEMENT="$(awk -F= '/^enforcement=/{print $2}' "${LOCKFILE}")" # strict|permissive
ALLOW_FAIL_OPEN="$(awk -F= '/^allow_fail_open=/{print $2}' "${LOCKFILE}")" # 0|1

[[ -z "${EXPECTED_SCRIPT_SHA}" || -z "${EXPECTED_SPEC_SHA}" ]] && die "Lockfile incomplete (needs script_sha256 and spec_sha256)."
[[ -f "${SPEC_PATH}" ]] || die "Spec not found: ${SPEC_PATH}"

ACTUAL_SCRIPT_SHA="$(sha256 "${SCRIPT_DIR}/ethics-guard.sh")"
ACTUAL_SPEC_SHA="$(sha256 "${SPEC_PATH}")"

if [[ "${EXPECTED_SCRIPT_SHA}" != "${ACTUAL_SCRIPT_SHA}" ]]; then
  die "ethics-guard.sh integrity mismatch (expected ${EXPECTED_SCRIPT_SHA}, got ${ACTUAL_SCRIPT_SHA})"
fi
if [[ "${EXPECTED_SPEC_SHA}" != "${ACTUAL_SPEC_SHA}" ]]; then
  die "Spec integrity mismatch for ${SPEC_PATH} (expected ${EXPECTED_SPEC_SHA}, got ${ACTUAL_SPEC_SHA})"
fi

# -------- Runner sanity checks --------
if [[ "$(id -u)" -eq 0 ]]; then
  warn "Running as root is discouraged; drop privileges in systemd (User=runner)."
fi

# Ensure tools present
require_cmd python3
# yq optional; Python path is primary
if ! python3 - <<'PY' >/dev/null 2>&1
import sys
import yaml, json
PY
then
  die "Python missing PyYAML. Install 'pyyaml' for spec parsing."
fi

# -------- Compliance validation --------
# Prefer your repo scripts; fall back to built-in minimal validator
CHECKER="${REPO_ROOT}/bithub/scripts/check_env_compliance.py"
EXPORTER="${REPO_ROOT}/bithub/scripts/export_compliance_report.py"

TMP_JSON="${ARTIFACTS_DIR}/compliance_raw.json"
mkdir -p "$(dirname "${OUT_JSON}")"

VALIDATION_STATUS="unknown"
VALIDATION_MSG=""

if [[ -x "${CHECKER}" || -f "${CHECKER}" ]]; then
  info "Using repo checker: ${CHECKER}"
  set +e
  python3 "${CHECKER}" --spec "${SPEC_PATH}" --aln-mode "${ALN_MODE}" --export "${OUT_ALN}" 1>"${TMP_JSON}.log" 2>&1
  rc=$?
  set -e
  if [[ $rc -ne 0 ]]; then
    VALIDATION_STATUS="fail"
    VALIDATION_MSG="Repo checker failed (exit ${rc}). See ${TMP_JSON}.log"
  else
    VALIDATION_STATUS="pass"
    VALIDATION_MSG="Repo checker passed."
  fi
else
  info "Repo checker not found; using minimal inline validator."
  # Minimal: verify required keys exist in YAML
  python3 - "$SPEC_PATH" <<'PY' >"${TMP_JSON}"
import sys, json, yaml, os, time
p=sys.argv[1]
data=yaml.safe_load(open(p,'r',encoding='utf-8'))
required=['policies','controls','exceptions']
missing=[k for k in required if k not in data]
status='pass' if not missing else 'fail'
print(json.dumps({'status':status,'missing':missing,'spec':os.path.basename(p)},ensure_ascii=False))
PY
  if jq -e . >/dev/null 2>&1 < "${TMP_JSON}"; then :; else require_cmd jq; fi
  if [[ "$(jq -r .status "${TMP_JSON}")" == "pass" ]]; then
    VALIDATION_STATUS="pass"
    VALIDATION_MSG="Inline validator: required keys present."
  else
    VALIDATION_STATUS="fail"
    VALIDATION_MSG="Inline validator: missing keys $(jq -r '.missing|join(",")' "${TMP_JSON}")"
  fi
  # Emit minimal ALN report
  cat >"${OUT_ALN}" <<ALN
; ALN compliance export (minimal)
@REPORT {
  suite: "EthicsGuard",
  status: "${VALIDATION_STATUS}",
  spec: "$(basename "${SPEC_PATH}")",
  timestamp: "$(now_iso())"
}
ALN
fi

# If exporter exists, build a rich JSON; else build inline JSON
if [[ -f "${EXPORTER}" ]]; then
  info "Using exporter: ${EXPORTER}"
  set +e
  python3 "${EXPORTER}" --output "${OUT_JSON}" 1>>"${TMP_JSON}.log" 2>&1
  rc2=$?
  set -e
  if [[ $rc2 -ne 0 ]]; then
    warn "Exporter failed (exit ${rc2}); falling back to inline JSON."
    cat >"${OUT_JSON}" <<JSON
{
  "status": "${VALIDATION_STATUS}",
  "message": "${VALIDATION_MSG}",
  "spec_path": "${SPEC_PATH}",
  "timestamp": "$(now_iso())",
  "host": "$(hostname_safe)",
  "commit": "$(git_commit)",
  "node_id": "${BIT_HUB_NODE_ID:-unknown}",
  "aln_report": "$(basename "${OUT_ALN}")"
}
JSON
  fi
else
  cat >"${OUT_JSON}" <<JSON
{
  "status": "${VALIDATION_STATUS}",
  "message": "${VALIDATION_MSG}",
  "spec_path": "${SPEC_PATH}",
  "timestamp": "$(now_iso())",
  "host": "$(hostname_safe)",
  "commit": "$(git_commit)",
  "node_id": "${BIT_HUB_NODE_ID:-unknown}",
  "aln_report": "$(basename "${OUT_ALN}")"
}
JSON
fi

# -------- Evidence signing (optional, anti-tamper) --------
EVIDENCE_HASH_FILE="${OUT_JSON}.sha3"
EVIDENCE_SIG_FILE="${OUT_JSON}.sig"
if command -v sha3sum >/dev/null 2>&1; then
  sha3sum "${OUT_JSON}" | awk '{print $1}' > "${EVIDENCE_HASH_FILE}"
else
  if command -v openssl >/dev/null 2>&1; then
    openssl dgst -sha3-512 -r "${OUT_JSON}" | awk '{print $1}' > "${EVIDENCE_HASH_FILE}"
  fi
fi

if [[ -n "${BIT_HUB_SIGNING_KEY:-}" ]] && command -v openssl >/dev/null 2>&1; then
  # BIT_HUB_SIGNING_KEY: path to PEM private key in a protected location
  openssl dgst -sha256 -sign "${BIT_HUB_SIGNING_KEY}" -out "${EVIDENCE_SIG_FILE}" "${OUT_JSON}" || warn "Signing failed"
fi

# -------- Enforcement logic --------
MODE="${ENFORCEMENT:-${ALN_MODE}}"
ALLOW_OPEN="${ALLOW_FAIL_OPEN:-0}"
STATUS="$(jq -r .status "${OUT_JSON}" 2>/dev/null || echo "${VALIDATION_STATUS}")"

info "Compliance status=${STATUS} mode=${MODE} allow_fail_open=${ALLOW_OPEN}"

if [[ "${STATUS}" == "fail" ]]; then
  # Always emit artifacts, then decide exit behavior
  cp -f "${OUT_JSON}" "${ARTIFACTS_DIR}/"
  cp -f "${OUT_ALN}" "${ARTIFACTS_DIR}/" || true
  [[ -f "${EVIDENCE_HASH_FILE}" ]] && cp -f "${EVIDENCE_HASH_FILE}" "${ARTIFACTS_DIR}/"
  [[ -f "${EVIDENCE_SIG_FILE}" ]] && cp -f "${EVIDENCE_SIG_FILE}" "${ARTIFACTS_DIR}/"

  if [[ "${MODE}" == "strict" && "${ALLOW_OPEN}" != "1" ]]; then
    die "Compliance breach in strict mode. Artifacts written to ${ARTIFACTS_DIR}."
  else
    warn "Compliance breach in permissive/fail-open mode. Proceeding but logged."
    exit 0
  fi
fi

# Success path: copy artifacts too (audit trail even when green)
cp -f "${OUT_JSON}" "${ARTIFACTS_DIR}/"
cp -f "${OUT_ALN}" "${ARTIFACTS_DIR}/" || true
[[ -f "${EVIDENCE_HASH_FILE}" ]] && cp -f "${EVIDENCE_HASH_FILE}" "${ARTIFACTS_DIR}/"
[[ -f "${EVIDENCE_SIG_FILE}" ]] && cp -f "${EVIDENCE_SIG_FILE}" "${ARTIFACTS_DIR}/"

info "Compliance passed. Artifacts in ${ARTIFACTS_DIR}."
