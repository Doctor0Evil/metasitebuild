#!/usr/bin/env bash
set -euo pipefail
mkdir -p .bit/out .bit/ml
POLICY=".bit/ml/policies.yml"
TOKEN=".bit/tokens/runner_bitcoin_token.json"

# Defaults
ALLOW_DEPLOY=true
RISK_SCORE=0.05

# Inputs
compscore=$(jq -r '.compscore // 900' "$TOKEN" 2>/dev/null || echo 900)
profanity=$(jq -r '.profanity // 1' "$TOKEN" 2>/dev/null || echo 1)

# Policy seed (idempotent)
if [ ! -f "$POLICY" ]; then
  cat > "$POLICY" <<'YML'
version: 1
rules:
  - id: high-risk-large-change
    when:
      changed_files_gt: 50
    then:
      risk_score: 0.35
      allow_deploy: false
  - id: low-compscore-caution
    when:
      compscore_lt: 750
    then:
      risk_score: 0.25
      allow_deploy: false
  - id: adult-content-tighten
    when:
      profanity_ge: 3
    then:
      risk_score: 0.20
      allow_deploy: true
fallback:
  risk_score: 0.05
  allow_deploy: true
YML
fi

# Gather repo signals
changed=$(git diff --name-only HEAD~1..HEAD 2>/dev/null | wc -l | tr -d ' ')
changed=${changed:-0}

# Optional supervised ML endpoint
if [ -n "${ML_ENDPOINT:-}" ] && command -v curl >/dev/null 2>&1; then
  resp="$(curl -sS -X POST "$ML_ENDPOINT" -H "Authorization: Bearer ${ML_API_KEY:-}" \
    -H 'Content-Type: application/json' \
    -d "{\"changed\":$changed,\"compscore\":$compscore,\"profanity\":$profanity}" || true)"
  ml_allow=$(echo "$resp" | jq -r '.allow_deploy // empty' 2>/dev/null || true)
  ml_risk=$(echo "$resp" | jq -r '.risk_score // empty' 2>/dev/null || true)
  [ -n "$ml_allow" ] && ALLOW_DEPLOY="$ml_allow"
  [ -n "$ml_risk" ] && RISK_SCORE="$ml_risk"
fi

# Else-if-then-so fallback policy (deterministic)
if [ "$changed" -gt "$(yq '.rules[] | select(.id=="high-risk-large-change").when.changed_files_gt' "$POLICY")" ]; then
  ALLOW_DEPLOY="false"; RISK_SCORE="0.35"
elif [ "$compscore" -lt "$(yq '.rules[] | select(.id=="low-compscore-caution").when.compscore_lt' "$POLICY")" ]; then
  ALLOW_DEPLOY="false"; RISK_SCORE="0.25"
elif [ "$profanity" -ge "$(yq '.rules[] | select(.id=="adult-content-tighten").when.profanity_ge' "$POLICY")" ]; then
  ALLOW_DEPLOY="true"; RISK_SCORE="0.20"
else
  ALLOW_DEPLOY="$(yq -r '.fallback.allow_deploy' "$POLICY")"
  RISK_SCORE="$(yq -r '.fallback.risk_score' "$POLICY")"
fi

echo "ALLOW_DEPLOY=$ALLOW_DEPLOY"
echo "RISK_SCORE=$RISK_SCORE"
echo "::notice ::ML policy eval: allow=$ALLOW_DEPLOY risk=$RISK_SCORE compscore=$compscore profanity=$profanity changed=$changed"
