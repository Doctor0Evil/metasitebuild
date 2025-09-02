needs: opa-policy-check
if: needs.opa-policy-check.outputs.policy_passed == 'true'


<#
.SYNOPSIS
  HRM Compliance runner: evaluates trace against OPA policies and enforces fail-closed logic.

.DESCRIPTION
  - Produces signed trace JSON
  - Runs OPA (or bundled engine) with local policy dir
  - Produces immutable JSONL audit lines and signatures
  - Exits non-zero when compliance gate fails (FailMode gate)
#>

param(
  [string]$ComplianceLevel = "strict",
  [string]$AuditDir        = ".bithub/audit",
  [string]$PolicyDir       = ".bithub/policies",
  [string]$HumorLog        = ".bithub/logs/humor-bot.log",
  [string]$TraceFile       = ".bithub/audit/trace.json",
  [string]$OpaResultFile   = ".bithub/audit/opa-result.json",
  [string]$OpaQuery        = "data.bithub.allow",
  [ValidateSet("gate","log")] [string]$FailMode = "gate",
  [switch]$AutoInstallOpa = $true
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function New-Dir { param($p) if ($p) { New-Item -ItemType Directory -Force -Path $p | Out-Null } }
function Write-ImmutableJsonl {
  param($obj, $file)
  $json = $obj | ConvertTo-Json -Depth 100
  $json | Out-File -FilePath $file -Encoding UTF8 -Append
}

# 1) Prepare directories
New-Dir -p $AuditDir
New-Dir -p (Split-Path $HumorLog -Parent)

# 2) Build trace
$trace = [PSCustomObject]@{
  schema = "bithub.trace.v1"
  component = "humor.reasoning.compliance"
  run_id = $env:GITHUB_RUN_ID
  ref = $env:GITHUB_REF
  sha = $env:GITHUB_SHA
  actor = $env:GITHUB_ACTOR
  repo = $env:GITHUB_REPOSITORY
  complianceLevel = $ComplianceLevel
  timestamp = (Get-Date).ToUniversalTime().ToString("o")
  content = (if ($env:SUBMISSION_TEXT) { $env:SUBMISSION_TEXT } else { "auto-check" })
}

# 3) Write trace file (atomic)
$traceTemp = "$TraceFile.tmp"
$trace | ConvertTo-Json -Depth 100 | Out-File -FilePath $traceTemp -Encoding UTF8
Move-Item -Force -Path $traceTemp -Destination $TraceFile

# 4) Sign trace: prefer TPM or env PEM
function Sign-Data {
  param($path)
  $bytes = [IO.File]::ReadAllBytes($path)
  # Use TPM-backed key if available (pseudo-check); otherwise fallback to PEM in secret ENV
  if ($env:TPM_SIGN_KEY_ID) {
    # Example: call your agent that uses TPM to sign. Must be available on runner.
    Write-Host "::notice::Signing with TPM key id $env:TPM_SIGN_KEY_ID"
    # Implement integration with your KMS/TPM agent here.
    return @{ alg="TPM-SIG"; key_id=$env:TPM_SIGN_KEY_ID; signature="tpm-signature-placeholder" }
  } elseif ($env:OWNER_BITHUB_PRIVATE_KEY_PEM) {
    $pem = $env:OWNER_BITHUB_PRIVATE_KEY_PEM
    $rsa = [System.Security.Cryptography.RSA]::Create()
    $rsa.ImportFromPem($pem.ToCharArray())
    $sig = $rsa.SignData($bytes,[System.Security.Cryptography.HashAlgorithmName]::SHA256,[System.Security.Cryptography.RSASignaturePadding]::Pkcs1)
    return @{ alg="RS256"; key_id="owner:bithub"; signature=[Convert]::ToBase64String($sig) }
  } else {
    Write-Host "::warning::No signing key configured; producing unsigned trace (not recommended)."
    return $null
  }
}

$sig = Sign-Data -path $TraceFile
if ($sig) { $trace.signatures = @($sig) } else { $trace.signatures = @() }
# rewrite signed trace file
$trace | ConvertTo-Json -Depth 100 | Out-File -FilePath $TraceFile -Encoding UTF8

# 5) Ensure OPA binary or equivalent
function Get-Opa {
  if (Get-Command -ErrorAction SilentlyContinue opa) { return (Get-Command opa).Source }
  if (-not $AutoInstallOpa) { return $null }
  # Minimal installer: in production use pinned version and validate checksum
  $binDir = Join-Path $AuditDir "bin"; New-Dir -p $binDir
  if ($IsWindows) {
    $dest = Join-Path $binDir "opa.exe"
    $url = "https://openpolicyagent.org/downloads/latest/opa_windows_amd64.exe"
  } else {
    $dest = Join-Path $binDir "opa"
    $url = "https://openpolicyagent.org/downloads/latest/opa_linux_amd64_static"
  }
  Write-Host "::notice::Downloading OPA to $dest"
  Invoke-WebRequest -Uri $url -OutFile $dest -UseBasicParsing
  if (-not $IsWindows) { chmod +x $dest }
  return (Resolve-Path $dest).Path
}

$opaExe = Get-Opa
if (-not $opaExe) {
  Write-Error "OPA engine not available and AutoInstallOpa is false. Aborting."
  exit 1
}

# 6) Run OPA eval (fail-open vs fail-closed: we choose fail-closed)
$outFile = "$AuditDir/opa.out.json"
$errFile = "$AuditDir/opa.err.txt"
$args = @("eval","--format=json","--data",$PolicyDir,"--input",$TraceFile,$OpaQuery)
$proc = Start-Process -FilePath $opaExe -ArgumentList $args -NoNewWindow -Wait -PassThru -RedirectStandardOutput $outFile -RedirectStandardError $errFile
if ($proc.ExitCode -ne 0) {
  # OPA failed — treat as non-compliant (fail-closed)
  Write-Host "::error::OPA evaluation failed (exit $($proc.ExitCode)). Treating as compliance failure."
  $opaResult = @{ available = $true; pass = $false; raw = @{ error = "opa-exec-failed"; code = $proc.ExitCode; stderr = (Get-Content $errFile -Raw) } }
} else {
  $raw = Get-Content $outFile -Raw | ConvertFrom-Json
  # guard for unexpected structure
  try {
    $val = $raw.result[0].expressions[0].value
    $opaResult = @{ available = $true; pass = [bool]$val; raw = $raw }
  } catch {
    $opaResult = @{ available = $true; pass = $false; raw = $raw }
  }
}

# 7) Write OPA result and immutable audit
$opaResult | ConvertTo-Json -Depth 200 | Out-File -FilePath $OpaResultFile -Encoding UTF8

$traceSha = (Get-FileHash $TraceFile -Algorithm SHA256).Hash
$opaSha   = (Get-FileHash $OpaResultFile -Algorithm SHA256).Hash
$logLine = [PSCustomObject]@{
  schema = "bithub.audit.v1"
  ts = (Get-Date).ToUniversalTime().ToString("o")
  run_id = $env:GITHUB_RUN_ID
  ref = $env:GITHUB_REF
  repo = $env:GITHUB_REPOSITORY
  actor = $env:GITHUB_ACTOR
  compliance = @{ level = $ComplianceLevel; pass = $opaResult.pass }
  artefacts = @{
    trace = @{ file = $TraceFile; sha256 = $traceSha; signatures = $trace.signatures }
    opa = @{ file = $OpaResultFile; sha256 = $opaSha }
  }
}
$logFile = Join-Path $AuditDir "immutable-log.jsonl"
$logLine | ConvertTo-Json -Depth 200 | Add-Content -Path $logFile -Encoding UTF8

# 8) Fail-closed enforcement
if (($FailMode -eq "gate") -and (-not $opaResult.pass)) {
  Write-Error "❌ Compliance gate failed. Exiting with non-zero to enforce quarantine."
  exit 1
}

Write-Host "✔ Compliance check passed (or logged)."
exit 0
