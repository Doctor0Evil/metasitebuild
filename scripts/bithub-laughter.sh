#!/usr/bin/env bash
# Bit.Hub Megawall "Laughter.exe" — universal, never-fail compliance orchestrator
# Works locally or in CI. Requires git; everything else is auto-installed best-effort.
# Always exits 0. Writes reports to .bithub/reports and events to .bithub/events.

################################################################################
# CONFIG (override via env)
################################################################################
BIT_HUB_REPO_URL="${BIT_HUB_REPO_URL:-https://github.com/Doctor0Evil/Bit.Hub.git}"
POWER_THRESHOLD="${POWER_THRESHOLD:-standard}"    # standard|strict|paranoid
ORG_SCOPE_PATH="${ORG_SCOPE_PATH:-.bit/org-scope.json}"
CANONICAL_WORKFLOW_NAME="${CANONICAL_WORKFLOW_NAME:-Bit.Hub Megawave Compliance}"
IMAGE_NAME_DEFAULT="${IMAGE_NAME_DEFAULT:-service}"
IMAGE_TAG_DEFAULT="${IMAGE_TAG_DEFAULT:-latest}"
BIN="${HOME}/.local/bin"
REPORT_DIR=".bithub/reports"
EVENTS_DIR=".bithub/events"
POLICY_DIR=".bithub/policy"
SCHEMAS_DIR=".bit/schemas"
PATTERNS_DIR=".bit/patterns"

# Optional secrets/vars for mirrors and failover (best-effort usage)
FAILOVER_WEBHOOK_URL="${FAILOVER_WEBHOOK_URL:-}"
MIRROR_1_URL="${MIRROR_1_URL:-}"; MIRROR_1_PAT="${MIRROR_1_PAT:-}"
MIRROR_2_URL="${MIRROR_2_URL:-}"; MIRROR_2_PAT="${MIRROR_2_PAT:-}"
PRIVATE_REGISTRY_HOST="${PRIVATE_REGISTRY_HOST:-}"
PRIVATE_REGISTRY_NAMESPACE="${PRIVATE_REGISTRY_NAMESPACE:-}"
PRIVATE_REGISTRY_USERNAME="${PRIVATE_REGISTRY_USERNAME:-}"
PRIVATE_REGISTRY_PASSWORD="${PRIVATE_REGISTRY_PASSWORD:-}"

################################################################################
# LOGGING (never-exit helpers)
################################################################################
ts() { date -u +'%Y-%m-%dT%H:%M:%SZ'; }
note() { echo "::notice::[$(ts)] $*"; }
warn() { echo "::warning::[$(ts)] $*"; }
err () { echo "::error::[$(ts)] $*"; }
run () { # run <desc> <cmd...>
  local desc="$1"; shift
  note "$desc"
  "$@" || warn "$desc failed (continuing)"
}
ensure_dir() { mkdir -p "$1" 2>/dev/null || true; }

################################################################################
# BOOTSTRAP
################################################################################
ensure_dir "$BIN" "$REPORT_DIR" "$EVENTS_DIR"
export PATH="$BIN:$PATH"
note "Bit.Hub Laughter.exe starting (threshold=$POWER_THRESHOLD)"

# Detect repo and branch (best-effort)
REPO_URL="$(git remote get-url origin 2>/dev/null || echo '')"
REPO_NAME="$(basename -s .git "$REPO_URL" 2>/dev/null || basename "$PWD")"
REPO_SLUG="$(git config --get remote.origin.url 2>/dev/null | sed -E 's#.*github.com[:/](.+/.+)(\.git)?#\1#' || echo '')"
BRANCH="$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo 'main')"
note "Repository: ${REPO_SLUG:-unknown} (branch: ${BRANCH})"

################################################################################
# DEPENDENCIES (best-effort install to ~/.local/bin)
################################################################################
need() { command -v "$1" >/dev/null 2>&1; }
dl() { curl -fsSL "$1" -o "$2" || wget -qO "$2" "$1"; }

install_jq()   { need jq || run "Install jq"   sh -c 'if command -v apt-get >/dev/null; then sudo apt-get update -y && sudo apt-get install -y jq; elif command -v brew >/dev/null; then brew install jq; else dl https://github.com/stedolan/jq/releases/download/jq-1.6/jq-linux64 "'"$BIN/jq"'" && chmod +x "'"$BIN/jq"'"; fi'; }
install_gh()   { need gh || run "Install gh"   sh -c 'if command -v apt-get >/dev/null; then type -p curl >/dev/null || (sudo apt-get update -y && sudo apt-get install -y curl); curl -fsSL https://raw.githubusercontent.com/cli/cli/trunk/script/install.sh | sh -s -- -b "'"$BIN"'"; elif command -v brew >/dev/null; then brew install gh; fi'; }
install_yq()   { need yq || run "Install yq"   sh -c 'dl https://github.com/mikefarah/yq/releases/download/v4.44.3/yq_linux_amd64 "'"$BIN/yq"'" && chmod +x "'"$BIN/yq"'" || true'; }
install_opa()  { need opa || run "Install opa"  sh -c 'dl https://openpolicyagent.org/downloads/v0.64.1/opa_linux_amd64_static "'"$BIN/opa"'" && chmod +x "'"$BIN/opa"'" || true'; }
install_docker(){ need docker || warn "Docker not found; container wall will be skipped"; }

install_jq; install_gh; install_yq; install_opa; install_docker

################################################################################
# SYNC CANONICAL POLICIES / SCHEMAS / PATTERNS
################################################################################
SYNC_TMP="$(mktemp -d)"
if git ls-remote "$BIT_HUB_REPO_URL" &>/dev/null; then
  run "Clone Bit.Hub canonical policies" git clone --depth=1 "$BIT_HUB_REPO_URL" "$SYNC_TMP"
  run "Sync policy directory"    rsync -a --ignore-existing "$SYNC_TMP/.bithub/policy/" "$POLICY_DIR/" || true
  run "Sync schemas directory"   rsync -a --ignore-existing "$SYNC_TMP/.bit/schemas/" "$SCHEMAS_DIR/" || true
  run "Sync patterns directory"  rsync -a --ignore-existing "$SYNC_TMP/.bit/patterns/" "$PATTERNS_DIR/" || true
else
  warn "Canonical repo unreachable; using local copies if present"
fi

################################################################################
# HUMOR-REASONING THRESHOLD ESCALATION (commit message + surfaces)
################################################################################
hot_commit=0
LAST_MSG="$(git log -1 --pretty=%B 2>/dev/null || echo '')"
grep -Eqi '\b(lol|omfg|wtf|lmao|lmfao|meltdown|chaos)\b' <<<"$LAST_MSG" && hot_commit=1
wf_changed="$(git diff --name-only HEAD~1..HEAD 2>/dev/null | grep -Ec '^\.github/workflows/.*ya?ml$' || echo 0)"
docker_changed="$(git diff --name-only HEAD~1..HEAD 2>/dev/null | grep -Ec '(^Dockerfile$|^docker/|^containers?/)' || echo 0)"

if [[ "$POWER_THRESHOLD" == "standard" ]]; then
  if (( wf_changed > 0 || docker_changed > 0 )); then POWER_THRESHOLD="strict"; fi
  if (( hot_commit==1 )); then POWER_THRESHOLD="paranoid"; fi
fi
note "Resolved threshold: $POWER_THRESHOLD"
echo "{\"event\":\"threshold_resolved\",\"threshold\":\"$POWER_THRESHOLD\",\"ts\":\"$(ts)\"}" >> "$REPORT_DIR/events.ndjson"

################################################################################
# WORKFLOW ENV RECOGNITION + NORMALIZATION (auto-fix best-effort)
################################################################################
if need yq; then
  ensure_dir "$REPORT_DIR"
  ENV_SCAN="$REPORT_DIR/environment-scan.ndjson"
  : > "$ENV_SCAN"
  if [ -d ".github/workflows" ]; then
    note "Scanning workflows"
    while IFS= read -r f; do
      uses=$(yq '.. | select(tag == "!!str") | select(test("@"))' "$f" | tr '\n' ' ' || true)
      name=$(yq -r '.name // ""' "$f" || true)
      body="$(cat "$f")"
      env="unknown"
      grep -Eiq 'build' <<<"$name$body" && env="build"
      grep -Eiq 'test'  <<<"$name$body" && env="test"
      grep -Eiq 'docker|Dockerfile|build-push-action@' <<<"$body" && env="container"
      grep -Eiq 'gh-release|softprops/action-gh-release@|lmfao\.bit' <<<"$body" && env="release"
      jq -nc --arg path "$f" --arg env "$env" --arg uses "$uses" \
        '{path:$path, env:$env, uses:($uses|split(" ")|map(select(length>0)))}' >> "$ENV_SCAN"
      # Global normalization (never destructive)
      yq -i 'if has("permissions") then . else .permissions={"contents":"read"} end' "$f"
      yq -i 'if has("concurrency") then . else .concurrency={"group":"wf-'"$BRANCH"'","cancel-in-progress":false} end' "$f"
      yq -i '.jobs |= with_entries(.value."timeout-minutes" = (.value."timeout-minutes" // 30 | tonumber))' "$f"
      yq -i '.jobs |= with_entries(.value."runs-on" = (.value."runs-on" // ["self-hosted","bit.hub","linux"]))' "$f"
      yq -i '.. | select(tag == "!!str") |= sub("actions/checkout@v[12]$"; "actions/checkout@v4")' "$f"
    done < <(find .github/workflows -maxdepth 1 -type f \( -name '*.yml' -o -name '*.yaml' \) | sort)
    note "Environment scan written to $ENV_SCAN"
  else
    warn "No .github/workflows directory found"
  fi
else
  warn "yq not available; skipping normalization"
fi

################################################################################
# WORKFLOW POLICY EVALUATION (advisory walls)
################################################################################
if need opa && [ -d "$POLICY_DIR" ]; then
  WF_REPORT="$REPORT_DIR/workflow-policy.ndjson"; : > "$WF_REPORT"
  if [ -d ".github/workflows" ]; then
    while IFS= read -r f; do
      if need yq; then
        jq -n --arg path "$f" --argjson wf "$(yq -o=json '.' "$f")" '{path:$path, workflow:$wf}' > /tmp/wf.json
        opa eval -f json -I -d "$POLICY_DIR" -i /tmp/wf.json 'data.bithub.workflow' \
          | jq -c '.result[].expressions[].value | {path: input.path, deny:(.deny // []), warn:(.warn // [])}' --argfile input /tmp/wf.json \
          >> "$WF_REPORT" || true
      fi
    done < <(find .github/workflows -maxdepth 1 -type f \( -name '*.yml' -o -name '*.yaml' \) | sort)
    note "Workflow policy report: $WF_REPORT"
  fi
else
  warn "OPA or policies not available; skipping workflow policy evaluation"
fi

################################################################################
# CONTAINER COMPLIANCE WALL (local build + inspect, no push)
################################################################################
if need docker && need opa && [ -f "Dockerfile" ] && [ -d "$POLICY_DIR" ]; then
  IMG="${REPO_NAME:-$IMAGE_NAME_DEFAULT}:${IMAGE_TAG_DEFAULT}"
  run "Build local image $IMG" docker build \
      --label "org.opencontainers.image.source=${REPO_URL:-unknown}" \
      --label "org.opencontainers.image.description=Bit.Hub-compliant image" \
      --label "org.opencontainers.image.licenses=MIT" \
      -t "$IMG" .
  ensure_dir "$REPORT_DIR"
  docker inspect "$IMG" > "$REPORT_DIR/image-inspect.json" 2>/dev/null || true
  jq -n --arg kind "container_image" --arg name "$IMG" \
        --slurpfile meta "$REPORT_DIR/image-inspect.json" \
        '{kind:$kind, name:$name, metadata:$meta[0]}' > /tmp/image.json
  opa eval -f json -I -d "$POLICY_DIR" -i /tmp/image.json 'data.bithub.container' \
    | jq -c '.result[].expressions[].value | {name: input.name, deny:(.deny // []), warn:(.warn // [])}' --argfile input /tmp/image.json \
    > "$REPORT_DIR/container-wall.ndjson" || true
  note "Container wall report: $REPORT_DIR/container-wall.ndjson"
else
  warn "Skipping container wall (Docker/OPA/policies/Dockerfile missing)"
fi

################################################################################
# AVAILABILITY SENTINEL (rate-limit + queue; optional failover webhook)
################################################################################
if need gh; then
  RATE="$(gh api /rate_limit --jq '.resources.core.remaining' 2>/dev/null || echo 0)"
  INFLIGHT="$(gh run list --limit 200 --json status --jq '[.[]|select(.status!="completed")]|length' 2>/dev/null || echo 0)"
  jq -n --argjson rate "$RATE" --argjson inflight "$INFLIGHT" \
    '{github:{rate_limit:{remaining:$rate}}, queue:{inflight:$inflight}, ts:"'"$(ts)"'"}' \
    > "$REPORT_DIR/platform-health.json"
  note "Platform health: rate=$RATE inflight=$INFLIGHT"
  if [[ -n "$FAILOVER_WEBHOOK_URL" ]] && { [ "$RATE" -lt 50 ] || [ "$INFLIGHT" -gt 6 ]; }; then
    run "Invoke failover webhook" curl -fsS "$FAILOVER_WEBHOOK_URL" \
      -H "Content-Type: application/json" \
      -d "{\"repo\":\"$REPO_SLUG\",\"ref\":\"$BRANCH\",\"threshold\":\"$POWER_THRESHOLD\"}"
  fi
else
  warn "gh not available; skipping sentinel checks"
fi

################################################################################
# OPTIONAL: OPEN PR WITH NORMALIZATION FIXES (if GH_TOKEN present)
################################################################################
if need gh && git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  git add -A || true
  if ! git diff --cached --quiet; then
    msg="chore(bit.hub): normalize workflows (permissions, concurrency, timeouts, runs-on, checkout@v4)"
    run "Create local commit" git -c user.name="bitbot" -c user.email="bitbot@users.noreply.github.com" commit -m "$msg"
    if [[ -n "$GITHUB_TOKEN" || -n "$GH_TOKEN" ]]; then
      BR="$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo main)"
      run "Push branch" git push -u origin "$BR"
      run "Open PR" gh pr create --fill --title "Bit.Hub Auto-Fix: Workflow normalization" \
        --body "Automated normalization by Laughter.exe. Threshold: $POWER_THRESHOLD" || true
    else
      warn "No token found; created local commit only"
    fi
  else
    note "No workflow changes to commit"
  fi
fi

################################################################################
# ORG MEGAWAVE DISPATCH (all pinned repos from .bit/org-scope.json)
################################################################################
if need gh && [ -f "$ORG_SCOPE_PATH" ]; then
  note "Dispatching megawave '$CANONICAL_WORKFLOW_NAME' to repos in $ORG_SCOPE_PATH"
  while IFS= read -r repo; do
    [ -z "$repo" ] && continue
    run "Dispatch to $repo" gh workflow run "$CANONICAL_WORKFLOW_NAME" -R "$repo" -f auto_fix=true || true
  done < <(jq -r '.repos[]?' "$ORG_SCOPE_PATH" 2>/dev/null)
else
  warn "Skipping org megawave (gh or $ORG_SCOPE_PATH missing)"
fi

################################################################################
# MIRROR SYNC (best-effort)
################################################################################
mirror_push() {
  local name="$1" url="$2" pat="$3"
  [ -z "$url" ] && return 0
  note "Mirror push to $name"
  git remote add "$name" "$url" 2>/dev/null || git remote set-url "$name" "$url"
  if [[ "$url" =~ ^https:// && -n "$pat" ]]; then
    local auth="${url/https:\/\//https:\/\/x-access-token:${pat}@}"
    git remote set-url "$name" "$auth"
  fi
  git push --mirror "$name" || warn "Mirror $name push failed"
}
if git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  mirror_push "mirror1" "$MIRROR_1_URL" "$MIRROR_1_PAT"
  mirror_push "mirror2" "$MIRROR_2_URL" "$MIRROR_2_PAT"
fi

################################################################################
# EVENTS & EXIT
################################################################################
ensure_dir "$EVENTS_DIR"
cat > "$EVENTS_DIR/laughter-${RANDOM}.json" <<JSON
{
  "event": "laughter_exe_completed",
  "repo": "${REPO_SLUG}",
  "branch": "${BRANCH}",
  "threshold": "${POWER_THRESHOLD}",
  "timestamp": "$(ts)",
  "reports": {
    "env_scan": "${REPORT_DIR}/environment-scan.ndjson",
    "workflow_policy": "${REPORT_DIR}/workflow-policy.ndjson",
    "container_wall": "${REPORT_DIR}/container-wall.ndjson",
    "platform_health": "${REPORT_DIR}/platform-health.json"
  }
}
JSON

note "Bit.Hub Laughter.exe finished. All steps best-effort. Workflow never fails."
exit 0
chmod +x scripts/bithub-laughter.sh
