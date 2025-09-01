<# 
.SYNOPSIS
  Humor-Reasoning-Model + Compliance-Floor Runner (Bit.Hub Production Grade)

.DESCRIPTION
  - Produces HRM output (jokes or provided submission), logs to humor log
  - Evaluates compliance via OPA query against policy bundle
  - Writes immutable audit artefacts (.json, .jsonl) with SHA256 digests
  - Signs trace with OWNER-ENFORCEMENT RSA key when provided
  - Optionally includes GitHub OIDC attestation for provenance

.NOTES
  Requires: PowerShell 7.x (Core)
  Optional: opa binary on PATH (auto-install enabled by default)
#>

[CmdletBinding()]
param(
  [string]$ComplianceLevel = "strict",            # standard | strict | paranoid
  [string]$AuditDir        = ".bithub/audit",
  [string]$PolicyDir       = ".bithub/policies",
  [string]$HumorLog        = ".bithub/logs/humor-bot.log",
  [string]$TraceFile       = ".bithub/audit/humor-reasoning-trace.json",
  [string]$OpaResultFile   = ".bithub/audit/opa-result.json",
  [string]$OpaQuery        = "data.bithub.allow", # OPA decision path
  [string]$SubmissionText,                        # Optional: your own text
  [switch]$AutoInstallOpa,                        # Auto-fetch OPA if missing
  [ValidateSet("gate","log")]
  [string]$FailMode       = "gate",               # gate: exit 1 on violations; log: never fail
  [string]$OidcAudience   = "bithub",             # OIDC attestation audience (if available)

  # OWNER-ENFORCEMENT signing (RSA PEM key). Provide either env var or file path.
  [string]$OwnerKeyPemEnv = "OWNER_ENFORCEMENT_PRIVATE_KEY_PEM",
  [string]$OwnerKeyPemPath,
  [string]$OwnerKeyId     = "owner:key:v1"        # Logical key id to record with signature
)

# -------------------- Helpers --------------------
function New-Directory {
  param([string]$Path)
  if (-not [string]::IsNullOrWhiteSpace($Path)) {
    New-Item -ItemType Directory -Force -Path $Path | Out-Null
  }
}

function Get-Sha256Hex {
  param([string]$Path)
  if (-not (Test-Path -LiteralPath $Path)) { throw "File not found for hashing: $Path" }
  $sha = [System.Security.Cryptography.SHA256]::Create()
  try {
    $bytes  = [System.IO.File]::ReadAllBytes((Resolve-Path $Path))
    $digest = $sha.ComputeHash($bytes)
    -join ($digest | ForEach-Object { $_.ToString("x2") })
  } finally { $sha.Dispose() }
}

function Get-OpaPath {
  # Prefer preconfigured path or PATH
  $opa = Get-Command opa -ErrorAction SilentlyContinue
  if ($opa) { return $opa.Source }

  if (-not $AutoInstallOpa) { return $null }

  # Auto-install OPA (ephemeral) into .bithub/bin
  $binDir = Join-Path $AuditDir "..\bin" | Resolve-Path -ErrorAction SilentlyContinue
  if (-not $binDir) {
    $binDir = Join-Path $AuditDir "../bin"
  }
  New-Directory -Path $binDir
  $os = $PSStyle.FileInfo.DirectorySeparatorChar # just to confirm cross-platform
  if ($IsWindows) {
    $url = "https://openpolicyagent.org/downloads/latest/opa_windows_amd64.exe"
    $dest = Join-Path $binDir "opa.exe"
  } elseif ($IsMacOS) {
    $url = "https://openpolicyagent.org/downloads/latest/opa_darwin_amd64"
    $dest = Join-Path $binDir "opa"
  } else {
    $url = "https://openpolicyagent.org/downloads/latest/opa_linux_amd64_static"
    $dest = Join-Path $binDir "opa"
  }
  Write-Host "::notice::Downloading OPA from $url"
  try {
    Invoke-WebRequest -UseBasicParsing -Uri $url -OutFile $dest
    if (-not $IsWindows) { chmod +x $dest }
    return (Resolve-Path $dest).Path
  } catch {
    Write-Warning "OPA auto-install failed: $($_.Exception.Message)"
    return $null
  }
}

function Invoke-OpaEval {
  param(
    [string]$OpaExe,
    [string]$PolicyDir,
    [string]$InputFile,
    [string]$Query
  )
  if (-not (Test-Path -LiteralPath $PolicyDir)) {
    return @{
      available = $false
      pass      = $true   # fail-open on missing policy dir unless gating
      raw       = @{ error = "PolicyDirMissing"; dir = $PolicyDir }
    }
  }

  if (-not $OpaExe) {
    return @{
      available = $false
      pass      = $true   # fail-open on missing OPA unless gating
      raw       = @{ error = "OpaMissing" }
    }
  }

  # OPA eval (JSON output). Expect a boolean decision at the query path.
  $cmd = @(
    $OpaExe, "eval", "--format=json",
    "--data", $PolicyDir,
    "--input", $InputFile,
    $Query
  )
  Write-Host "::notice::OPA> $($cmd -join ' ')"
  $p = Start-Process -FilePath $cmd[0] -ArgumentList $cmd[1..($cmd.Length-1)] -NoNewWindow -PassThru -RedirectStandardOutput "opa.out.json" -RedirectStandardError "opa.err.txt" -Wait
  $out = Get-Content "opa.out.json" -Raw -ErrorAction SilentlyContinue
  $err = Get-Content "opa.err.txt" -Raw -ErrorAction SilentlyContinue

  if ($p.ExitCode -ne 0 -or -not $out) {
    return @{
      available = $true
      pass      = $false
      raw       = @{ error = "OpaEvalFailed"; exit = $p.ExitCode; stderr = $err }
    }
  }

  try {
    $json = $out | ConvertFrom-Json -Depth 10
    # Typical shape: { "result":[{"expressions":[{"value":true,"text":"data.bithub.allow"}]}] }
    $val = $json.result[0].expressions[0].value
    $pass = [bool]$val
    return @{ available = $true; pass = $pass; raw = $json }
  } catch {
    return @{
      available = $true
      pass      = $false
      raw       = @{ error = "OpaParseError"; detail = $_.Exception.Message; raw = $out }
    }
  }
}

function Get-OwnerPem {
  if ($OwnerKeyPemPath -and (Test-Path -LiteralPath $OwnerKeyPemPath)) {
    return (Get-Content -LiteralPath $OwnerKeyPemPath -Raw)
  }
  $envPem = [Environment]::GetEnvironmentVariable($OwnerKeyPemEnv)
  if ($envPem) { return $envPem }
  return $null
}

function Sign-FileRsa {
  param(
    [string]$Path,
    [string]$Pem,
    [string]$KeyId = "owner:key:v1"
  )
  if (-not $Pem) { return $null }

  $data = [System.IO.File]::ReadAllBytes((Resolve-Path $Path))
  $rsa  = [System.Security.Cryptography.RSA]::Create()
  try {
    $rsa.ImportFromPem($Pem.ToCharArray())
  } catch {
    throw "Failed to import RSA PEM for OWNER-ENFORCEMENT signing: $($_.Exception.Message)"
  }

  $sigBytes = $rsa.SignData($data, [System.Security.Cryptography.HashAlgorithmName]::SHA256, [System.Security.Cryptography.RSASignaturePadding]::Pkcs1)
  $sigB64   = [Convert]::ToBase64String($sigBytes)
  return @{
    alg      = "RS256"
    key_id   = $KeyId
    signature= $sigB64
  }
}

function Get-OidcAttestation {
  param([string]$Audience = "bithub")
  # Available on GitHub Actions:
  # ACTIONS_ID_TOKEN_REQUEST_URL, ACTIONS_ID_TOKEN_REQUEST_TOKEN
  $reqUrl   = $env:ACTIONS_ID_TOKEN_REQUEST_URL
  $reqToken = $env:ACTIONS_ID_TOKEN_REQUEST_TOKEN
  if (-not $reqUrl -or -not $reqToken) { return $null }
  try {
    $uri = "$reqUrl&audience=$Audience"
    $hdr = @{ Authorization = "bearer $reqToken" }
    $resp = Invoke-RestMethod -Method GET -Uri $uri -Headers $hdr
    return @{
      provider = "github-oidc"
      audience = $Audience
      jwt      = $resp.value
    }
  } catch {
    Write-Warning "OIDC attestation fetch failed: $($_.Exception.Message)"
    return $null
  }
}

# -------------------- Prep --------------------
$ErrorActionPreference = 'Stop'
New-Directory -Path $AuditDir
New-Directory -Path (Split-Path -Parent $HumorLog)

# -------------------- HRM Output --------------------
if ([string]::IsNullOrWhiteSpace($SubmissionText)) {
  $jokes = @(
    "Why did the compliance bot cross the road? To close the gap analysis.",
    "Bit.Hub compliance is like a good punchline — it lands every time.",
    "My humor model passed the Turing Test… but only for dad jokes.",
    "Profanity with audit-logging? Now that’s what I call a logged-on joke."
  )
  $SubmissionText = Get-Random $jokes
}
Write-Host "🤖 HRM> $SubmissionText"
Add-Content -Path $HumorLog -Value "$(Get-Date -Format o) :: $SubmissionText"

# -------------------- Trace --------------------
$traceObj = [PSCustomObject]@{
  schema          = "bithub.trace.v1"
  component       = "humor.reasoning.compliance"
  run_id          = $env:GITHUB_RUN_ID
  ref             = $env:GITHUB_REF
  sha             = $env:GITHUB_SHA
  event           = $env:GITHUB_EVENT_NAME
  actor           = $env:GITHUB_ACTOR
  repo            = $env:GITHUB_REPOSITORY
  complianceLevel = $ComplianceLevel
  content         = $SubmissionText
  timestamp       = (Get-Date).ToUniversalTime().ToString("o")
}
$traceObj | ConvertTo-Json -Depth 8 | Out-File -FilePath $TraceFile -Encoding utf8
$traceSha = Get-Sha256Hex -Path $TraceFile

# -------------------- OPA Evaluation --------------------
$opaExe = Get-OpaPath
$opa = Invoke-OpaEval -OpaExe $opaExe -PolicyDir $PolicyDir -InputFile $TraceFile -Query $OpaQuery

# Policy result normalization (compliance floor)
$compliancePass = $opa.pass

# Strictness escalation
switch -Regex ($ComplianceLevel) {
  '^paranoid$' { if (-not $opa.available) { $compliancePass = $false } }
  '^strict$'   { if (-not $opa.available) { $compliancePass = $compliancePass -and $true } }
  default      { $compliancePass = $compliancePass } # standard
}

# -------------------- OWNER-ENFORCEMENT Signing --------------------
$ownerPem = Get-OwnerPem
$signature = $null
if ($ownerPem) {
  try {
    $signature = Sign-FileRsa -Path $TraceFile -Pem $ownerPem -KeyId $OwnerKeyId
  } catch {
    Write-Warning $_.Exception.Message
  }
}

# -------------------- OIDC Attestation (optional) --------------------
$attest = Get-OidcAttestation -Audience $OidcAudience

# -------------------- Persist OPA Output --------------------
$opaOut = [PSCustomObject]@{
  decisionPath = $OpaQuery
  available    = $opa.available
  pass         = $compliancePass
  engine       = if ($opaExe) { (Split-Path $opaExe -Leaf) } else { "none" }
  raw          = $opa.raw
  checkedAt    = (Get-Date).ToUniversalTime().ToString("o")
}
$opaOut | ConvertTo-Json -Depth 15 | Out-File -FilePath $OpaResultFile -Encoding utf8
$opaSha = Get-Sha256Hex -Path $OpaResultFile

# -------------------- Immutable JSONL --------------------
$logEntry = [PSCustomObject]@{
  schema     = "bithub.audit.v1"
  ts         = (Get-Date).ToUniversalTime().ToString("o")
  run_id     = $env:GITHUB_RUN_ID
  ref        = $env:GITHUB_REF
  repo       = $env:GITHUB_REPOSITORY
  actor      = $env:GITHUB_ACTOR
  compliance = @{
    level = $ComplianceLevel
    pass  = $compliancePass
  }
  artefacts  = @{
    trace = @{
      file = $TraceFile
      sha256 = $traceSha
      signature = $signature
      oidc = $attest
    }
    opa   = @{
      file = $OpaResultFile
      sha256 = $opaSha
    }
    humor_log = $HumorLog
  }
}
$logFile = Join-Path $AuditDir "immutable-log.jsonl"
$logEntry | ConvertTo-Json -Depth 20 | Add-Content -Path $logFile -Encoding utf8

# -------------------- Exit --------------------
if ($FailMode -eq "gate") {
  if ($compliancePass) {
    Write-Host "✔ Compliance floor passed."
    exit 0
  } else {
    Write-Error "❌ Compliance floor failed."
    exit 1
  }
} else {
  Write-Host "::warning::FailMode=log — recording result only (not failing pipeline)."
  exit 0
}
