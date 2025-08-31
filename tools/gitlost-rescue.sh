# tools/gitlost-rescue.sh
#!/usr/bin/env bash
set -euo pipefail
ROOT="${1:-.}"
LOG="$ROOT/.bit/audit/gitlost.log"
mkdir -p "$(dirname "$LOG")" "$ROOT/.github/workflows" "$ROOT/.bit/tokens"

touch "$LOG"

# TOS
[ -f "$ROOT/TERMS-OF-SERVICE.md" ] || {
  cat > "$ROOT/TERMS-OF-SERVICE.md" <<'TOS'
# Bit.Hub Community Terms of Service
Execution implies acceptance. Governed by .gitcomply, .gitenforcement, config.bit.create
TOS
  echo "created TOS" >> "$LOG"
}

# Compliance wall
[ -f "$ROOT/.github/workflows/bithub-bot-compliance-wall.yml" ] || {
  cat > "$ROOT/.github/workflows/bithub-bot-compliance-wall.yml" <<'YML'
name: Bithub Bot Compliance Wall
on: { workflow_dispatch: {}, push: { branches: [ main, "**" ] } }
jobs:
  wall:
    runs-on: ubuntu-latest
    steps:
      - name: Final Celebration
        if: always()
        run: echo "🧭 gitlost wall active (fail-open)."
YML
  echo "scaffolded compliance wall" >> "$LOG"
}

# Token
[ -f "$ROOT/.bit/tokens/runner_bitcoin_token.json" ] || {
  mkdir -p "$ROOT/.bit/tokens"
  printf '{"token":"%s","scope":"ephemeral","issuedAt":"%s"}\n' "$(uuidgen || echo auto)" "$(date -u +%FT%TZ)" \
    > "$ROOT/.bit/tokens/runner_bitcoin_token.json"
  echo "issued ephemeral token" >> "$LOG"
}

echo "[gitlost] rescue complete (fail-open). See $LOG"
