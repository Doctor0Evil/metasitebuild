#!/usr/bin/env bash
# Bit.Hub Laughter.exe — Runner Compliance Fixer
# Purpose: Sync rules from Doctor0Evil/Bit.Hub and apply them to this repo's workflows.

set -euo pipefail

BIT_HUB_REPO_URL="${BIT_HUB_REPO_URL:-https://github.com/Doctor0Evil/Bit.Hub.git}"
POWER_THRESHOLD="${POWER_THRESHOLD:-standard}" # standard|strict|paranoid
BIN="$HOME/.local/bin"
POLICY_DIR=".bithub/policy"
CFG_FILE=".gitcomply"
CREATE_FILE=".bit/config.bit.create"
REPORT_DIR=".bithub/reports"

mkdir -p "$BIN" "$POLICY_DIR" "$REPORT_DIR"

note() { echo "::notice::[Laughter.exe] $*"; }
warn() { echo "::warning::[Laughter.exe] $*"; }

# --- Install deps (best-effort) ---
install_yq() { command -v yq >/dev/null || { curl -fsSL -o "$BIN/yq" https://github.com/mikefarah/yq/releases/download/v4.44.3/yq_linux_amd64 && chmod +x "$BIN/yq"; }; }
install_opa(){ command -v opa >/dev/null || { curl -fsSL -o "$BIN/opa" https://openpolicyagent.org/downloads/v0.64.1/opa_linux_amd64_static && chmod +x "$BIN/opa"; }; }
install_yq; install_opa
export PATH="$BIN:$PATH"

# --- Sync canonical rules ---
TMP=$(mktemp -d)
if git ls-remote "$BIT_HUB_REPO_URL" &>/dev/null; then
  note "Cloning Bit.Hub rules..."
  git clone --depth=1 "$BIT_HUB_REPO_URL" "$TMP"
  cp -f "$TMP/.gitcomply" "$CFG_FILE"
  mkdir -p .bit
  cp -f "$TMP/.bit/config.bit.create" "$CREATE_FILE"
  rsync -a --ignore-existing "$TMP/.bithub/policy/" "$POLICY_DIR/"
else
  warn "Cannot reach Bit.Hub repo; using existing local rules."
fi

# --- Apply config.bit.create defaults to workflows ---
if [ -f "$CREATE_FILE" ] && command -v yq >/dev/null; then
  note "Applying config.bit.create defaults..."
  while IFS= read -r wf; do
    yq -i '.permissions = (.permissions // load("'"$CREATE_FILE"'").defaults.workflow.permissions)' "$wf"
    yq -i '.concurrency = (.concurrency // load("'"$CREATE_FILE"'").defaults.workflow.concurrency)' "$wf"
    yq -i '.jobs |= with_entries(.value."timeout-minutes" = (.value."timeout-minutes" // load("'"$CREATE_FILE"'").defaults.workflow.timeout-minutes))' "$wf"
    yq -i '.. | select(tag == "!!str") |= sub("actions/checkout@v[12]$"; load("'"$CREATE_FILE"'").defaults.workflow.checkout_version)' "$wf"
  done < <(find .github/workflows -type f \( -name '*.yml' -o -name '*.yaml' \))
else
  warn "No config.bit.create found or yq missing; skipping default injection."
fi

# --- Evaluate workflows against Bit.Hub policy ---
if command -v opa >/dev/null && [ -d "$POLICY_DIR" ]; then
  note "Evaluating workflows against Bit.Hub policy..."
  REPORT="$REPORT_DIR/workflow-policy.ndjson"; : > "$REPORT"
  while IFS= read -r wf; do
    jq -n --arg path "$wf" --argjson wf_json "$(yq -o=json '.' "$wf")" \
      '{path:$path, workflow:$wf_json}' > /tmp/input.json
    opa eval -f json -I -d "$POLICY_DIR" -i /tmp/input.json 'data.bithub.workflow' \
      | jq -c '.result[].expressions[].value | {path: input.path, deny:(.deny // []), warn:(.warn // [])}' --argfile input /tmp/input.json \
      >> "$REPORT" || true
  done < <(find .github/workflows -type f \( -name '*.yml' -o -name '*.yaml' \))
  note "Policy report written to $REPORT"
else
  warn "OPA or policies missing; skipping policy evaluation."
fi

# --- Commit & PR changes (if any) ---
if git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  git add .github/workflows "$CFG_FILE" "$CREATE_FILE" || true
  if ! git diff --cached --quiet; then
    git -c user.name="bitbot" -c user.email="bitbot@users.noreply.github.com" commit -m "chore(bit.hub): apply runner compliance fixes"
    if [ -n "${GITHUB_TOKEN:-}" ]; then
      BRANCH="bitbot/compliance-fixes"
      git push -u origin HEAD:"$BRANCH" || true
      gh pr create --fill --title "Bit.Hub Compliance Fixes" --body "Automated fixes from Laughter.exe" || true
    else
      warn "No GITHUB_TOKEN; committed locally only."
    fi
  else
    note "No compliance changes needed."
  fi
fi

note "Laughter.exe compliance pass complete (threshold=$POWER_THRESHOLD)."
exit 0
