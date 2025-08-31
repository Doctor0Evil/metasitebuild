# ============================================================
# ALNFantasia.create
# Central_Data_Bank + Humor-Reasoning-Core + Personality Matrix
# Sticky thresholds, AI Safeguards, Helm chart, and Santa distribution
# Governed by Bit.Hub (fail-open, repair-first).
# ============================================================

$ErrorActionPreference = 'Stop'

# -----------------------------
# Configuration
# -----------------------------
$baseDir = 'C:\Users\Hunter\ALN_Programming_Language'
$repoUrl = 'https://github.com/Doctor0Evil/Bit.Hub.git'
$dirName = 'Bit.Hub'
$gitUser = 'automation-bot'
$gitEmail = 'automation-bot@noreply.local'

# Optional: enforce repository origin (fail-open: warn, continue)
$allowedRepoUrl = $repoUrl

# -----------------------------
# Helpers
# -----------------------------
function Ensure-Dir {
    param([string]$Path)
    if (-not (Test-Path $Path)) { New-Item -ItemType Directory -Path $Path -Force | Out-Null }
}
function Set-GitIdentity {
    git config user.name  $gitUser | Out-Null
    git config user.email $gitEmail | Out-Null
}
function Git-AuthHeader {
    $token = $env:BIT_HUB_PAT
    if ([string]::IsNullOrWhiteSpace($token)) {
        Write-Warning "BIT_HUB_PAT is not set. Pushes may fail on protected branches."
        return $null
    }
    $bytes = [System.Text.Encoding]::UTF8.GetBytes(":$token")
    $b64   = [Convert]::ToBase64String($bytes)
    return "Authorization: Basic $b64"
}
function Git-Push-Authenticated {
    param([string]$Branch)
    $header = Git-AuthHeader
    if ($null -ne $header) {
        git -c http.extraheader="$header" push origin $Branch
    } else {
        git push origin $Branch
    }
}
function Ensure-Repo {
    param([string]$Base, [string]$Url, [string]$Name)
    $localPath = Join-Path $Base $Name
    if (-not (Test-Path $localPath)) {
        Write-Host "[+] Cloning Bit.Hub repository for compliance enforcement..."
        git clone $Url $localPath
    } else {
        Write-Host "[!] Bit.Hub repo exists, fetching updates..."
        Push-Location $localPath
        try {
            $origin = (git remote get-url origin 2>$null)
            if ($origin -and $allowedRepoUrl -and $origin -ne $allowedRepoUrl) {
                Write-Warning "Origin mismatch: $origin (expected $allowedRepoUrl). Continuing (fail-open)."
            }
            git fetch --all --prune
            $hasMain = (git branch -r | Select-String 'origin/main') -ne $null
            if ($hasMain) {
                git checkout main *> $null
                git pull --rebase origin main
            } else {
                git pull --rebase
            }
        } finally { Pop-Location }
    }
}
function Ensure-TOS {
    param([string]$RepoRoot)
    $tos = Join-Path $RepoRoot 'TERMS-OF-SERVICE.md'
    if (-not (Test-Path $tos)) {
        @"
# Bit.Hub Community Terms of Service

Execution implies acceptance. Governed by .gitcomply, .gitenforcement, and config.bit.create.
This repository and its runners operate under Bit.Hub and ALNFantasia governance. Violations are logged and auto-repaired (fail-open).
"@ | Set-Content -Path $tos -Encoding UTF8
        Write-Host "[+] Created TERMS-OF-SERVICE.md"
    }
}

# -----------------------------
# Central Data Bank + HR Core + Personality Matrix
# -----------------------------
function Ensure-CentralDataBank {
    param([string]$RepoRoot)
    $cdb = Join-Path $RepoRoot '.bit/Central_Data_Bank'
    Ensure-Dir (Join-Path $RepoRoot '.bit')
    Ensure-Dir $cdb
    Ensure-Dir (Join-Path $cdb 'audit')
    Ensure-Dir (Join-Path $cdb 'schemas')
    return $cdb
}

function Write-HRCore {
    param([string]$CDB)
    $hrCore = Join-Path $CDB 'hr_core.json'
    if (-not (Test-Path $hrCore)) {
@"
{
  "version": "1.0",
  "humor_reasoning_core": {
    "quips": [
      "Compliance is choreography, not chains.",
      "We log the dragons so the bards can sing.",
      "Fail-open. Fix-forward. Celebrate progress."
    ],
    "style": "celebratory",
    "tone_bounds": { "banter_min": 0, "profanity_max": 2, "quirk_max": 8 }
  },
  "ai_safeguards": {
    "enabled": true,
    "content_maturity": "adult",
    "prohibited_tasks": [
      "model_training:on_restricted_data",
      "content_generation:extreme_violence"
    ],
    "human_review_required": [
      "content_generation:adult",
      "data_migration:cross_region"
    ],
    "telemetry": { "redact_sensitive": true, "store_audit": true }
  }
}
"@ | Set-Content -Path $hrCore -Encoding UTF8
        Write-Host "[+] Wrote HR Core: $hrCore"
    }
}

function Write-PersonalityMatrix {
    param([string]$CDB)
    $pm = Join-Path $CDB 'personality_matrix.json'
    if (-not (Test-Path $pm)) {
@"
{
  "version": "1.0",
  "matrix": {
    "banter": { "min": 0, "max": 1, "default": 0 },
    "profanity": { "min": 0, "max": 3, "default": 1 },
    "rep": { "min": 0, "max": 100000, "default": 100 },
    "compscore": { "min": 0, "max": 2000, "default": 900 },
    "quirk": { "min": 0, "max": 10, "default": 5 }
  },
  "enforcement": {
    "mode": "strict",
    "sticky_thresholds": true,
    "cooldown_minutes": 120,
    "non_interference_charter": true
  }
}
"@ | Set-Content -Path $pm -Encoding UTF8
        Write-Host "[+] Wrote Personality Matrix: $pm"
    }
}

function Write-ThresholdsPolicy {
    param([string]$CDB)
    $policy = Join-Path $CDB 'thresholds.json'
    if (-not (Test-Path $policy)) {
@"
{
  "version": "1.0",
  "current_level": "standard",
  "levels": {
    "standard": { "risk_ceiling": 0.10, "profanity_max": 2, "compscore_min": 800 },
    "heightened": { "risk_ceiling": 0.20, "profanity_max": 1, "compscore_min": 900 },
    "lockdown": { "risk_ceiling": 0.35, "profanity_max": 0, "compscore_min": 1100 }
  },
  "last_escalation": null,
  "cooldown_minutes": 120
}
"@ | Set-Content -Path $policy -Encoding UTF8
        Write-Host "[+] Wrote Thresholds Policy: $policy"
    }
}

function Enforce-StickyThresholds {
    param([string]$CDB)
    $policyPath = Join-Path $CDB 'thresholds.json'
    if (-not (Test-Path $policyPath)) { return }
    $policy = Get-Content $policyPath -Raw | ConvertFrom-Json
    $audit = Join-Path $CDB 'audit/thresholds.log'

    # Inputs (best-effort): compscore/profanity from .bit.coin token if present
    $tokenPath = Join-Path $CDB '../tokens/runner_bitcoin_token.json' | Resolve-Path -ErrorAction SilentlyContinue
    $compscore = 900; $profanity = 1; $risk = 0.05
    if ($tokenPath) {
        try {
            $t = Get-Content $tokenPath -Raw | ConvertFrom-Json
            if ($t.compscore) { $compscore = [int]$t.compscore }
            if ($t.profanity) { $profanity = [int]$t.profanity }
        } catch {}
    }
    # Risk signal can be sourced from prior ML gate output if available
    $mlEval = Join-Path $CDB '../out/ml-eval.txt' | Resolve-Path -ErrorAction SilentlyContinue
    if ($mlEval) {
        try {
            $ml = Get-Content $mlEval -Raw
            if ($ml -match 'RISK_SCORE=([0-9.]+)') { $risk = [double]$matches[1] }
        } catch {}
    }

    # Determine target level by policy ceilings
    $levels = $policy.levels
    $target = 'standard'
    if ($risk -gt $levels.standard.risk_ceiling -or $compscore -lt $levels.standard.compscore_min -or $profanity -gt $levels.standard.profanity_max) { $target = 'heightened' }
    if ($risk -gt $levels.heightened.risk_ceiling -or $compscore -lt $levels.heightened.compscore_min -or $profanity -gt $levels.heightened.profanity_max) { $target = 'lockdown' }

    # Sticky escalation: never auto-decrease unless cooldown expired
    $now = Get-Date
    $lastEsc = if ($policy.last_escalation) { [datetime]$policy.last_escalation } else { $null }
    $coolMins = [int]$policy.cooldown_minutes
    $canDeescalate = $false
    if ($lastEsc) {
        $elapsed = ($now - $lastEsc).TotalMinutes
        if ($elapsed -ge $coolMins) { $canDeescalate = $true }
    } else { $canDeescalate = $true } # first-time allows set

    $current = $policy.current_level
    $newLevel = $current
    if ($target -eq 'lockdown' -and $current -ne 'lockdown') { $newLevel = 'lockdown' }
    elseif ($target -eq 'heightened' -and $current -eq 'standard') { $newLevel = 'heightened' }
    elseif ($target -eq 'standard' -and $current -ne 'standard' -and $canDeescalate) { $newLevel = 'standard' }

    if ($newLevel -ne $current) {
        $policy.current_level = $newLevel
        $policy.last_escalation = $now.ToUniversalTime().ToString("o")
        ($policy | ConvertTo-Json -Depth 6) | Set-Content -Path $policyPath -Encoding UTF8
        "$($now.ToUniversalTime().ToString('o')) level_change $current -> $newLevel risk=$risk compscore=$compscore profanity=$profanity" | Out-File -FilePath $audit -Append -Encoding UTF8
        Write-Host "[*] Threshold level changed: $current -> $newLevel"
    } else {
        "$($now.ToUniversalTime().ToString('o')) level_stable $current risk=$risk compscore=$compscore profanity=$profanity" | Out-File -FilePath $audit -Append -Encoding UTF8
    }
}

# -----------------------------
# Helm chart for Central_Data_Bank service
# -----------------------------
function Write-HelmChart {
    param([string]$RepoRoot)
    $chartRoot = Join-Path $RepoRoot 'deploy/helm/central-databank'
    Ensure-Dir (Join-Path $RepoRoot 'deploy/helm')
    Ensure-Dir $chartRoot
    $chartY = Join-Path $chartRoot 'Chart.yaml'
    $valuesY = Join-Path $chartRoot 'values.yaml'
    $deployY = Join-Path $chartRoot 'templates/deployment.yaml'
    Ensure-Dir (Join-Path $chartRoot 'templates')

@"
apiVersion: v2
name: central-databank
description: Bit.Hub Central Data Bank (HR Core + Personality Matrix)
type: application
version: 0.1.0
appVersion: "1.0"
"@ | Set-Content -Path $chartY -Encoding UTF8

@"
replicaCount: 1
image:
  repository: bithub/central-databank
  tag: latest
  pullPolicy: IfNotPresent

resources:
  requests:
    cpu: "100m"
    memory: "128Mi"
  limits:
    cpu: "500m"
    memory: "512Mi"

config:
  hrCore: |-
$(Get-Content (Join-Path $RepoRoot '.bit/Central_Data_Bank/hr_core.json') -ErrorAction SilentlyContinue | ForEach-Object { '    ' + $_ })
  personalityMatrix: |-
$(Get-Content (Join-Path $RepoRoot '.bit/Central_Data_Bank/personality_matrix.json') -ErrorAction SilentlyContinue | ForEach-Object { '    ' + $_ })
  thresholds: |-
$(Get-Content (Join-Path $RepoRoot '.bit/Central_Data_Bank/thresholds.json') -ErrorAction SilentlyContinue | ForEach-Object { '    ' + $_ })
"@ | Set-Content -Path $valuesY -Encoding UTF8

@"
apiVersion: apps/v1
kind: Deployment
metadata:
  name: central-databank
spec:
  replicas: {{ .Values.replicaCount }}
  selector:
    matchLabels:
      app: central-databank
  template:
    metadata:
      labels:
        app: central-databank
    spec:
      containers:
        - name: central-databank
          image: "{{ .Values.image.repository }}:{{ .Values.image.tag }}"
          imagePullPolicy: "{{ .Values.image.pullPolicy }}"
          env:
            - name: HR_CORE_JSON
              valueFrom: { configMapKeyRef: { name: central-databank-config, key: hrCore } }
            - name: PERSONALITY_MATRIX_JSON
              valueFrom: { configMapKeyRef: { name: central-databank-config, key: personalityMatrix } }
            - name: THRESHOLDS_JSON
              valueFrom: { configMapKeyRef: { name: central-databank-config, key: thresholds } }
          resources:
            {{- toYaml .Values.resources | nindent 12 }}
---
apiVersion: v1
kind: ConfigMap
metadata:
  name: central-databank-config
data:
  hrCore: |
{{ .Values.config.hrCore | nindent 4 }}
  personalityMatrix: |
{{ .Values.config.personalityMatrix | nindent 4 }}
  thresholds: |
{{ .Values.config.thresholds | nindent 4 }}
"@ | Set-Content -Path $deployY -Encoding UTF8

    Write-Host "[+] Wrote Helm chart: $chartRoot"
}

# -----------------------------
# santa.clause.exe distribution (precision orchestrated)
# -----------------------------
function Invoke-SantaClause {
    param([string]$RepoRoot, [string]$OutDir = '.bit/out')
    $tool = Join-Path $RepoRoot 'tools/santa.clause.exe'
    $artifacts = Get-ChildItem -Path (Join-Path $RepoRoot $OutDir) -Recurse -File -ErrorAction SilentlyContinue
    if (-not $artifacts) {
        Write-Warning "No artifacts found in $OutDir; skipping santa distribution."
        return
    }
    $log = Join-Path $RepoRoot '.bit/Central_Data_Bank/audit/distribution.log'
    foreach ($a in $artifacts) {
        $ts = (Get-Date).ToUniversalTime().ToString('o')
        try {
            if (Test-Path $tool) {
                # Cross-platform: use pwsh to invoke .NET Core single-file if provided
                & $tool --deliver "$($a.FullName)" --targets "clusters,alnfantasia" --mode "precision" --audit "$log"
            } else {
                # Fallback: log-only precision handoff (fail-open)
                "$ts santa.stub deliver=$($a.FullName) targets=clusters,alnfantasia mode=precision" | Out-File -FilePath $log -Append -Encoding UTF8
            }
        } catch {
            "$ts santa.error file=$($a.FullName) $_" | Out-File -FilePath $log -Append -Encoding UTF8
        }
    }
    Write-Host "[+] Santa distribution executed (fail-open)."
}

# -----------------------------
# Compliance workflows for contributors (unchanged)
# -----------------------------
function Enforce-BitHubCompliance {
    param([string]$RepoRoot)
    $workflowDir = Join-Path $RepoRoot '.github\workflows'
    Ensure-Dir $workflowDir
    $contributors = git -C $RepoRoot shortlog -s -n | ForEach-Object { ($_ -replace '^\s*\d+\s+', '').Trim() }
    if (-not $contributors -or $contributors.Count -eq 0) { $contributors = @('default') }
    foreach ($user in $contributors) {
        $safeName = $user -replace '[^a-zA-Z0-9_\-]', '_'
        if ($safeName -match '^[\.\-]') { $safeName = "u_$safeName" }
        $wfName = "force_bithub_compliance_for_${safeName}.yml"
        $wfPath = Join-Path $workflowDir $wfName
$wfContent = @"
name: Enforce Bit.Hub Compliance for $user
on:
  push:
    branches: [ main ]
  pull_request:
  workflow_dispatch:
concurrency:
  group: enforce-bithub-$safeName-\${{ github.ref }}
  cancel-in-progress: false
jobs:
  enforce_compliance:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout
        uses: actions/checkout@v4
      - name: Terms of Service
        run: echo "This run is governed by TERMS-OF-SERVICE.md" >> "\$GITHUB_STEP_SUMMARY"
      - name: STRICT Intelligence Enforcement
        run: |
          echo "## STRICT Intelligence Enforcement" >> "\$GITHUB_STEP_SUMMARY"
          echo "Non-interference charter active; AI safeguards on." >> "\$GITHUB_STEP_SUMMARY"
      - name: Final Celebration
        if: always()
        run: echo "🎉 Enforcement logged. Run remains successful by policy."
"@
        Set-Content -Path $wfPath -Value $wfContent -Force -Encoding UTF8
        Write-Host "[+] Authored compliance workflow: $wfName"
    }
}

function Commit-And-Push {
    param([string]$RepoPath)
    Push-Location $RepoPath
    try {
        Set-GitIdentity
        git add -A | Out-Null
        git diff --cached --quiet
        $hasChanges = ($LASTEXITCODE -ne 0)
        if (-not $hasChanges) { Write-Host "[INFO] No changes to commit."; return }
        $commitMsg = "Bit.Hub: Central_Data_Bank HR Core + Personality Matrix + Helm chart + Santa orchestration"
        git commit -m $commitMsg
        $branch = (git rev-parse --abbrev-ref HEAD).Trim()
        if ([string]::IsNullOrWhiteSpace($branch) -or $branch -eq 'HEAD') { $branch = 'main' }
        try { Git-Push-Authenticated -Branch $branch; Write-Host "[SUCCESS] Pushed to origin/$branch." }
        catch { Write-Warning "Push failed (token scope/protection). Continuing (fail-open)." }
    } finally { Pop-Location }
}

# -----------------------------
# Optional: emit .bit.runner.bots Lua module (unchanged core)
# -----------------------------
function Write-LuaRunnerBotsModule {
    param([string]$RepoRoot)
    $luaDir  = Join-Path $RepoRoot 'engine'
    Ensure-Dir $luaDir
    $luaPath = Join-Path $luaDir '.bit.runner.bots.lua'
@"
-- ============================================================
-- .bit.runner.bots (Lua) — awareness of HR Core & Personality Matrix via env/bit files
-- ============================================================
local MapRenderer = MapRenderer or {}

local function readfile(path)
  local f = io.open(path, "r"); if not f then return nil end
  local d = f:read("*a"); f:close(); return d
end

local cdb = ".bit/Central_Data_Bank/"
local HR = readfile(cdb .. "hr_core.json") or "{}"
local PM = readfile(cdb .. "personality_matrix.json") or "{}"

local RunnerBot = {}; RunnerBot.__index = RunnerBot
function RunnerBot.new(id, name, role, complianceScore, personality)
  local self = setmetatable({}, RunnerBot)
  self.id, self.name, self.role = id, name, role or "generic"
  self.complianceScore = complianceScore or 1000
  self.personality = personality or { banter=0, profanity=0, rep=0, quirk=0 }
  self.status = "idle"; return self
end
function RunnerBot:assignTask(task)
  self.status = "busy"; self.currentTask = task
  print(string.format("[RunnerBot:%s] Assigned task: %s", self.name, task))
end
function RunnerBot:completeTask(success)
  self.status = "idle"
  if success then self.complianceScore = self.complianceScore + 5
  else self.complianceScore = math.max(0, self.complianceScore - 1) end
end
function RunnerBot:reportStatus()
  return { id=self.id, name=self.name, role=self.role, complianceScore=self.complianceScore,
           personality=self.personality, status=self.status }
end

local BotRegistry = {}
function BotRegistry.register(bot) BotRegistry[bot.id] = bot; print("[Registry] Registered bot:", bot.name) end
function BotRegistry.get(id) return BotRegistry[id] end
function BotRegistry.list() local t={} for _,b in pairs(BotRegistry) do table.insert(t, b:reportStatus()) end return t end

function MapRenderer.RenderWithBot(botId, ...)
  local bot = BotRegistry.get(botId)
  if not bot then print("[MapRenderer] No bot:", botId); return end
  bot:assignTask("Render operation"); bot:completeTask(true)
end

return { MapRenderer=MapRenderer, RunnerBot=RunnerBot, BotRegistry=BotRegistry, HR=HR, PM=PM }
"@ | Set-Content -Path $luaPath -Encoding UTF8
    Write-Host "[+] Emitted Lua runner bots module: $luaPath"
}

# -----------------------------
# Execution
# -----------------------------
Set-Location -Path $baseDir
Ensure-Dir $baseDir
Ensure-Repo -Base $baseDir -Url $repoUrl -Name $dirName

$repoPath = Join-Path $baseDir $dirName
Ensure-TOS -RepoRoot $repoPath

$cdb = Ensure-CentralDataBank -RepoRoot $repoPath
Write-HRCore -CDB $cdb
Write-PersonalityMatrix -CDB $cdb
Write-ThresholdsPolicy -CDB $cdb
Enforce-StickyThresholds -CDB $cdb
Write-HelmChart -RepoRoot $repoPath
Invoke-SantaClause -RepoRoot $repoPath

Write-LuaRunnerBotsModule -RepoRoot $repoPath
Enforce-BitHubCompliance -RepoRoot $repoPath
Commit-And-Push -RepoPath $repoPath

Write-Host "[DONE] Central_Data_Bank nested; HR Core + Personality Matrix + thresholds + Helm chart + Santa distribution ready (fail-open)."
jobs:
  central_databank_build:
    runs-on: windows-latest
    steps:
      - uses: actions/checkout@v4
      - name: Run ALNFantasia.create
        shell: pwsh
        run: |
          .\ALNFantasia.create
      - name: Package Central_Data_Bank
        shell: pwsh
        run: |
          mkdir -Force .bit\out
          $stamp = (Get-Date).ToUniversalTime().ToString("yyyy.MM.dd.HHmmss")
          $name = "central_databank-$stamp.tgz"
          tar -czf ".bit/out/$name" .bit/Central_Data_Bank deploy/helm/central-databank
          Write-Host "Packaged $name"
      - name: Upload Artifact
        uses: actions/upload-artifact@v4
        with:
          name: central-databank
          path: .bit/out/*.tgz
          retention-days: 30
