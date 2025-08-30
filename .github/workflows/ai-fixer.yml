# Fixed/compatible workflow file for Genius Workflow All-Fixer
# Powershell-compliant YAML content with corrected indentation, steps, and shell usage for all Bit.Hub bots/policies

$workflowContent = @'
name: Genius Workflow All-Fixer

on:
  workflow_dispatch:
  push:
    branches: [ "main" ]
    paths:
      - ".bit/**"
      - ".github/workflows/**"

permissions:
  contents: write
  pull-requests: write

jobs:
  ensure-bit:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout repository
        uses: actions/checkout@v4
        with:
          fetch-depth: 0
          clean: false

      - name: Ensure .bit scaffold
        shell: bash
        run: |
          mkdir -p .bit/bots .bit/policies .bit/command-sheets
          [ -f .bit/README.md ] || echo "# .bit" > .bit/README.md
          [ -f .bit/policies/compliance.yml ] || cat > .bit/policies/compliance.yml <<'EOF'
version: 1
rules:
  - id: bots-manifests-present
    description: Require at least one bot manifest in .bit/bots
    severity: error
EOF

      - name: Ensure default bot manifests
        shell: bash
        run: |
          declare -A bots
          bots["git.bit"]="GitOps-style repo orchestration bot"
          bots["compliance.bit"]="Compliance enforcement and audit bot"
          bots["orchestrator.bit"]="Meta-orchestration and workflow generator bot"
          for bot in "${!bots[@]}"; do
            file=".bit/bots/$bot.yml"
            if [ ! -f "$file" ]; then
              cat > "$file" <<EOF
apiVersion: bitbot/v1
kind: Bot
metadata:
  name: $bot
  description: ${bots[$bot]}
spec:
  triggers: [push, pull_request]
  permissions:
    contents: write
    pull-requests: write
  actions:
    - type: sync
      target: manifests
    - type: validate
      target: workflows
EOF
            fi
          done

      - name: Commit and push if changes
        shell: bash
        run: |
          git config user.name "github-actions[bot]"
          git config user.email "41898282+github-actions[bot]@users.noreply.github.com"
          git add .bit .gitignore || true
          git diff --cached --quiet || git commit -m "Ensure .bit scaffold [skip ci]"
          git pull --rebase origin main
          git push origin main

      - name: Execute batch workflows from command-sheet
        env:
          GH_TOKEN: ${{ github.token }}
        shell: bash
        run: |
          if [ -f ".bit/command-sheets/batch_register_and_sync.yml" ]; then
            echo "📜 Reading batch command-sheet..."
            grep 'workflow:' .bit/command-sheets/batch_register_and_sync.yml | awk '{print $2}' | while read wf; do
              echo "🚀 Triggering $wf..."
              gh workflow run "$wf"
            done
          else
            echo "⚠️ No batch command-sheet found."
          fi
'@

# Ensure the workflows directory exists
$workflowDir = 'C:\Users\Hunter\ALN_Programming_Language\.github\workflows'
if (-not (Test-Path $workflowDir)) {
    New-Item -ItemType Directory -Force -Path $workflowDir | Out-Null
}

# Write the workflow file
Set-Content -Path (Join-Path $workflowDir 'genius_workflow_all_fixer.yml') -Value $workflowContent -Force -Encoding UTF8

Write-Host "✅ genius_workflow_all_fixer.yml created and fixed for Bit.Hub compliance/orchestration."
