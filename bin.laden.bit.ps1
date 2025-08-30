# Bit.Hub rapid initializer with BitBots teaching/supervision and safe restarts
# - Parent-safe file creation, manifest logging
# - ".bitbots" registry + teaching/supervision scripts
# - Workflows for teach+supervise and re-run of failing loops
# - Simulated logic placeholders; wire up your real analyzers when ready

[CmdletBinding()]
param(
  [switch]$DryRun
)

$ErrorActionPreference = 'Stop'
$baseDir = 'C:\Users\Hunter\ALN_Programming_Language'
Set-Location -Path $baseDir

# ------------------------------
# Helpers
# ------------------------------
$createdDirs   = New-Object System.Collections.ArrayList
$existingDirs  = New-Object System.Collections.ArrayList
$createdFiles  = New-Object System.Collections.ArrayList
$existingFiles = New-Object System.Collections.ArrayList

function Ensure-Dir {
  param([string]$Path)
  if (-not (Test-Path $Path)) {
    if ($PSBoundParameters.ContainsKey('DryRun')) { Write-Host "[DRY] Would create dir: $Path" }
    else {
      New-Item -ItemType Directory -Path $Path -Force | Out-Null
      [void]$createdDirs.Add($Path)
      Write-Host "[+] Directory created: $Path"
    }
  } else {
    [void]$existingDirs.Add($Path)
    Write-Host "[*] Directory exists: $Path"
  }
}

function Write-File {
  param(
    [string]$Path,
    [string]$Content
  )
  $parent = Split-Path -Parent $Path
  if ($parent) { Ensure-Dir $parent }
  if (-not (Test-Path $Path)) {
    if ($PSBoundParameters.ContainsKey('DryRun')) {
      Write-Host "[DRY] Would write file: $Path"
    } else {
      Set-Content -Path $Path -Value $Content -Force -Encoding UTF8
      [void]$createdFiles.Add($Path)
      Write-Host "[+] File created: $Path"
    }
  } else {
    [void]$existingFiles.Add($Path)
    Write-Host "[*] File exists: $Path"
  }
}

function Ensure-FileTree {
  param(
    [string[]]$Directories,
    [hashtable[]]$LeafFiles
  )
  foreach ($d in $Directories) { Ensure-Dir $d }
  foreach ($spec in $LeafFiles) { Write-File -Path $spec.Path -Content $spec.Content }
}

function Write-ManifestReport {
  param([string]$OutPath = '.bithub/initializer_report.json')
  $report = [PSCustomObject]@{
    timestamp      = (Get-Date).ToString('o')
    dryRun         = [bool]$DryRun
    createdDirs    = $createdDirs
    existingDirs   = $existingDirs
    createdFiles   = $createdFiles
    existingFiles  = $existingFiles
    bitbots        = Get-Content '.bitbots/registry.json' -Raw -ErrorAction SilentlyContinue
  }
  Write-File -Path $OutPath -Content ($report | ConvertTo-Json -Depth 6)
}

function Try-Invoke-BitBotInit {
  # Optional registration hook (no-op if python not found)
  try {
    $py = Get-Command python -ErrorAction Stop
    if ($null -ne $py -and -not $DryRun) {
      Write-Host "[*] Registering initializer with BitBot (simulated)..."
      python bithub/scripts/bitbot_api_init.py --event init --manifest .bithub/initializer_report.json 2>$null
    }
  } catch {
    Write-Host "[i] Python not available; skipping BitBot init hook."
  }
}

# ------------------------------
# Target structure
# ------------------------------
$dirs = @(
  '.bit', '.bitbots', '.bithub', '.github', '.github/workflows', '.github/actions',
  'aln', 'bithub/scripts', 'core', 'data/security', 'manifests', 'policy', 'rego', 'rego/policy',
  'scripts', 'src/core', 'templates', 'workflows', '.github/cache/github_bots', '.logs'
)

$files = @(
  @{ Path = '.bit/access_policy.rego';      Content = 'package bit.accesspolicy' },
  @{ Path = '.bit/permissions.rego';       Content = 'package bit.permissions' },
  @{ Path = '.bit/triggers.rego';          Content = 'package bit.triggers' },
  @{ Path = '.bit/steps.rego';             Content = 'package bit.steps' },
  @{ Path = '.bit/actions.rego';           Content = 'package bit.actions' },
  @{ Path = '.bit/artifacts.rego';         Content = 'package bit.artifacts' },
  @{ Path = '.bit/reusable.rego';          Content = 'package bit.reusable' },
  @{ Path = '.bit/runners.rego';           Content = 'package bit.runners' },
  @{ Path = '.bit/policy.json';            Content = '{ "effect": "permit", "notes": "BitHub openness with enforceable chokepoints" }' },
  @{ Path = '.bit/contract.aln';           Content = 'ALN_CONTRACT_ID: default' },
  @{ Path = '.bit/workflow_manifest.aln';  Content = 'workflow: manifest' },
  @{ Path = '.bit/.bit.yml';               Content = 'name: Bit config' },
  @{ Path = '.bit/.bithub.yml';            Content = 'name: BitHub config' },
  @{ Path = '.bit/ReadMe.md';              Content = '# BitHub ReadMe' },
  @{ Path = '.bit/SECURITY.md';            Content = '# BitHub Security' },
  @{ Path = '.bit/bitbot-integrations.yml';Content = 'name: BitBot Integrations' },
  @{ Path = '.bit/ml.workflow.bit.yml';    Content = 'name: ML Workflow Bit' },

  @{ Path = '.bitbots/registry.json'; Content = @'
{
  "teachers": [
    {
      "id": "teacher-aln-001",
      "scope": ["workflows", "compliance", "params"],
      "methods": ["detect", "correct", "explain", "rerun"]
    },
    {
      "id": "teacher-lisp-001",
      "scope": ["aln", "lisp", "meta-evolution"],
      "methods": ["triage", "auto-patch", "advise"]
    }
  ],
  "mappings": {
    "github.bots": {
      "policy": ".bit/policy.json",
      "regulatory": ".bitbot/regulatory.json",
      "corrections": "bithub/scripts/variable_parser_correction.py"
    }
  }
}
'@ },

  @{ Path = '.bithub/initializer_report.json'; Content = '{}' },

  # BitBot teacher/supervisor scripts (simulated but structured)
  @{ Path = 'bithub/scripts/bitbot_teacher.py'; Content = @'
import json, os, sys, time, glob
out = {"teachings": [], "violations": [], "reruns": []}
# Simulated: scan .logs for failures
for fp in glob.glob(".logs/*.json"):
    try:
        data = json.load(open(fp))
        if data.get("status") == "failure":
            out["violations"].append({"run_id": data.get("run_id"), "issue": data.get("issue","unknown")})
            out["teachings"].append({"lesson": "Use strict schema + parser-corrections", "target": data.get("bot","github.bot")})
            out["reruns"].append({"run_id": data.get("run_id")})
    except Exception:
        pass
json.dump(out, open("teacher_report.json","w"))
print("teacher_report=teacher_report.json")
'@ },

  @{ Path = 'bithub/scripts/bitbot_supervisor.py'; Content = @'
import json, sys, subprocess, os
# Simulated compile/test; replace with real build commands
status = {"compile":"ok","tests":"ok","restart":False}
# If a marker exists, simulate a failure that should trigger restart
if os.path.exists(".logs/force_fail.marker"):
    status["compile"] = "fail"
    status["restart"] = True
json.dump(status, open("supervisor_status.json","w"))
print("supervisor_status=supervisor_status.json")
'@ },

  @{ Path = 'bithub/scripts/ml_pattern_tracker.py'; Content = @'
import json, time, os
db = {"timestamp": time.time(), "patterns": ["retry-on-transient", "auto-correct-params", "teach-before-rerun"]}
json.dump(db, open(".bithub/ml_patterns.json","w"))
print("ml_patterns=.bithub/ml_patterns.json")
'@ },

  # Existing correction tool placeholder if not present
  @{ Path = 'bithub/scripts/variable_parser_correction.py'; Content = @'
import sys, json
# Simulated: pass-through copy with "corrected": true
src = sys.argv[1] if len(sys.argv) > 1 else "audit_report.json"
try:
    data = json.load(open(src))
except Exception:
    data = {"note": "no input; created default"}
data["corrected"] = True
json.dump(data, open("audit_report.fixed.json","w"))
print("output=audit_report.fixed.json")
'@ },

  # Optional compile check script for workflows
  @{ Path = 'scripts/compile_check.sh'; Content = @'
#!/usr/bin/env bash
set -euo pipefail
echo "Simulated compile OK"
'@ },

  # Workflows: Teach + supervise (auto-correct + rerun), plus rerun listener
  @{ Path = '.github/workflows/bitbot_teach_and_supervise.yml'; Content = @"
name: BitBot Teach and Supervise
permissions:
  contents: read
  actions: write
  checks: read
on:
  workflow_run:
    workflows: ["*"]
    types: [completed]
  schedule:
    - cron: "*/20 * * * *"
  workflow_dispatch:
concurrency:
  group: teacher-\${{ github.ref }}
  cancel-in-progress: false
jobs:
  teach_supervise:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout
        uses: actions/checkout@v4

      - name: Track ML Patterns
        run: python bithub/scripts/ml_pattern_tracker.py

      - name: Teach GitHub Bots (Analyze Failures and Propose Fixes)
        run: |
          python bithub/scripts/bitbot_teacher.py
          echo "teacher_report=teacher_report.json" >> "\$GITHUB_OUTPUT"

      - name: Apply Parser Corrections
        run: |
          if [ -f audit_report.json ]; then
            python bithub/scripts/variable_parser_correction.py audit_report.json
          else
            echo "{}" > audit_report.json
            python bithub/scripts/variable_parser_correction.py audit_report.json
          fi

      - name: Supervise Compile/Test and Decide Restart
        run: |
          bash scripts/compile_check.sh || true
          python bithub/scripts/bitbot_supervisor.py
          echo "supervisor_status=supervisor_status.json" >> "\$GITHUB_OUTPUT"

      - name: Read Supervisor Status
        id: status
        run: |
          python - <<'PY'
import json
d=json.load(open("supervisor_status.json"))
print(f"restart={str(d.get('restart',False)).lower()}")
PY

      - name: Dispatch Rerun for Failing Loops
        if: contains(steps.status.outputs.restart, 'true')
        env:
          GH_TOKEN: \${{ secrets.GITHUB_TOKEN }}
        run: |
          echo "Triggering repository_dispatch: bitbot_rerun"
          curl -sS -X POST \
            -H "Authorization: Bearer \${GH_TOKEN}" \
            -H "Accept: application/vnd.github+json" \
            https://api.github.com/repos/\${{ github.repository }}/dispatches \
            -d '{"event_type":"bitbot_rerun","client_payload":{"reason":"compile_fail","ts":"'\$(date +%s)'"}}'
"@ },

  @{ Path = '.github/workflows/bitbot_rerun.yml'; Content = @"
name: BitBot Rerun Listener
permissions:
  actions: write
  contents: read
on:
  repository_dispatch:
    types: [bitbot_rerun]
concurrency:
  group: rerun-\${{ github.ref }}
  cancel-in-progress: false
jobs:
  rerun_latest_failed:
    runs-on: ubuntu-latest
    steps:
      - name: Identify last failed run
        id: find
        env:
          GH_TOKEN: \${{ secrets.GITHUB_TOKEN }}
        run: |
          # Get last failed workflow run id (scoped to repo)
          curl -sS -H "Authorization: Bearer \${GH_TOKEN}" \
            "https://api.github.com/repos/\${{ github.repository }}/actions/runs?status=failure&per_page=1" > runs.json
          echo "run_id=\$(jq -r '.workflow_runs[0].id // empty' runs.json)" >> "\$GITHUB_OUTPUT"

      - name: Re-run target workflow
        if: steps.find.outputs.run_id != ''
        env:
          GH_TOKEN: \${{ secrets.GITHUB_TOKEN }}
        run: |
          echo "Re-running \${{ steps.find.outputs.run_id }}"
          curl -sS -X POST \
            -H "Authorization: Bearer \${GH_TOKEN}" \
            -H "Accept: application/vnd.github+json" \
            "https://api.github.com/repos/\${{ github.repository }}/actions/runs/\${{ steps.find.outputs.run_id }}/rerun"
"@ },

  # Existing compliance cache workflow (optional; complements teaching loop)
  @{ Path = '.github/workflows/bitbot_compliance_learning.yml'; Content = @"
name: BitBot Compliance Learning & Enforcement
permissions:
  contents: read
  actions: read
  checks: read
on:
  schedule:
    - cron: "0 * * * *"
  workflow_dispatch:
concurrency:
  group: compliance-\${{ github.ref }}
  cancel-in-progress: true
env:
  RETENTION_COUNT: "5"
jobs:
  compliance_audit:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout
        uses: actions/checkout@v4

      - name: Learn & Detect Non-Compliance (Simulated)
        run: echo "{}" > audit_report.json

      - name: Parse & Correct Violations
        run: python bithub/scripts/variable_parser_correction.py audit_report.json

      - name: Cache GitHub Bot State
        uses: actions/cache@v4
        with:
          path: .github/cache/github_bots
          key: github-bots-\${{ github.run_id }}
          restore-keys: |
            github-bots-

      - name: Persist Snapshot
        run: |
          mkdir -p .github/cache/github_bots
          ts="\$(date +%Y%m%dT%H%M%S)"
          cp audit_report.fixed.json ".github/cache/github_bots/botstate-\$ts.json"

      - name: Prune Old Cache Entries
        shell: bash
        run: |
          mkdir -p .github/cache/github_bots
          ls -1t .github/cache/github_bots | tail -n +\$(( \${RETENTION_COUNT} + 1 )) | xargs -I {} rm -rf ".github/cache/github_bots/{}" || true
"@ },

  # Ignore cached state in Git
  @{ Path = '.github/.gitignore'; Content = ".github/cache/\n" }
)

# ------------------------------
# Execute
# ------------------------------
Ensure-FileTree -Directories $dirs -LeafFiles $files
Write-ManifestReport -OutPath '.bithub/initializer_report.json'
Try-Invoke-BitBotInit

Write-Host "[SIMULATED] Initialization complete. Teaching, supervision, and rerun workflows are in place."
if ($DryRun) { Write-Host "[DRY] No files were modified." }
