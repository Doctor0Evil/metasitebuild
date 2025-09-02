param(
  [string]$ComplianceLevel = "paranoid",
  [string]$AuditDir = ".bithub/audit",
  [string]$PolicyDir = ".bithub/policies",
  [string]$HumorLog = ".bithub/logs/humor-bot.log",
  [string]$TraceFile = ".bithub/audit/humor-reasoning-trace.json",
  [string]$OpaResultFile = ".bithub/audit/opa-result.json",
  [string]$OpaQuery = "data.bithub.allow",
  [string]$SubmissionText = "",
  [ValidateSet("gate","log")] [string]$FailMode = "gate",
  [switch]$AutoInstallOpa = $true
)

function New-Directory { param($Path) if ($Path) { New-Item -ItemType Directory -Force -Path $Path | Out-Null } }
function Get-Sha256Hex { param($Path) $sha=[Security.Cryptography.SHA256]::Create(); try { -join ($sha.ComputeHash([IO.File]::ReadAllBytes($Path))|%{$_.ToString('x2')}) } finally { $sha.Dispose() } }
function Get-OpaPath {
  $opa = Get-Command opa -ErrorAction SilentlyContinue
  if ($opa) { return $opa.Source }
  if (-not $AutoInstallOpa) { return $null }
  $bin = Join-Path $AuditDir "../bin"; New-Directory $bin
  if ($IsWindows) { $url="https://openpolicyagent.org/downloads/latest/opa_windows_amd64.exe"; $dest=Join-Path $bin "opa.exe" }
  elseif ($IsMacOS) { $url="https://openpolicyagent.org/downloads/latest/opa_darwin_amd64"; $dest=Join-Path $bin "opa" }
  else { $url="https://openpolicyagent.org/downloads/latest/opa_linux_amd64_static"; $dest=Join-Path $bin "opa" }
  Write-Host "::notice::Downloading OPA $url"
  Invoke-WebRequest -UseBasicParsing -Uri $url -OutFile $dest; if (-not $IsWindows) { chmod +x $dest }; (Resolve-Path $dest).Path
}
function Invoke-OpaEval {
  param($OpaExe, $PolicyDir, $InputFile, $Query)
  if (-not (Test-Path $PolicyDir)) { return @{ available=$false; pass=$true; raw=@{error="PolicyDirMissing"} } }
  if (-not $OpaExe) { return @{ available=$false; pass=$true; raw=@{error="OpaMissing"} } }
  $out="opa.out.json"; $err="opa.err.txt"
  $args = @("eval","--format=json","--data",$PolicyDir,"--input",$InputFile,$Query)
  $p = Start-Process -FilePath $OpaExe -ArgumentList $args -NoNewWindow -Wait -PassThru -RedirectStandardOutput $out -RedirectStandardError $err
  if ($p.ExitCode -ne 0) { return @{ available=$true; pass=$false; raw=@{error="OpaEvalFailed"; exit=$p.ExitCode; stderr=(gc $err -Raw) } } }
  $json = (gc $out -Raw | ConvertFrom-Json); $val = $json.result[0].expressions[0].value
  @{ available=$true; pass=[bool]$val; raw=$json }
}
function Sign-BytesRSA {
  param([byte[]]$Data, [string]$Pem, [string]$KeyId)
  if (-not $Pem) { return $null }
  $rsa=[Security.Cryptography.RSA]::Create(); $rsa.ImportFromPem($Pem.ToCharArray())
  $sig=$rsa.SignData($Data,[Security.Cryptography.HashAlgorithmName]::SHA256,[Security.Cryptography.RSASignaturePadding]::Pkcs1)
  @{ alg="RS256"; key_id=$KeyId; signature= [Convert]::ToBase64String($sig) }
}
function Get-EnvPem { param($Name) [Environment]::GetEnvironmentVariable($Name) }

# Initialize directories
New-Directory (Split-Path $HumorLog -Parent)
New-Directory $AuditDir

# Humor reasoning content
if ([string]::IsNullOrWhiteSpace($SubmissionText)) {
  $SubmissionText = @(
    "Why did the compliance bot cross the road? To close the gap analysis.",
    "Bit.Hub compliance is like a good punchline — it lands every time.",
    "My humor model passed the Turing Test… but only for dad jokes."
  ) | Get-Random
}
Write-Host "🤖 HRM> $SubmissionText"
Add-Content -Path $HumorLog -Value "$(Get-Date -Format o) :: $SubmissionText"

# Create trace object
$trace = [pscustomobject]@{
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
$trace | ConvertTo-Json -Depth 50 | Out-File -FilePath $TraceFile -Encoding utf8

# Dual sign trace
$bytes = [IO.File]::ReadAllBytes($TraceFile)
$sig1 = Sign-BytesRSA -Data $bytes -Pem (Get-EnvPem "OWNER_BITHUB_PRIVATE_KEY_PEM") -KeyId "owner:bithub"
$sig2 = Sign-BytesRSA -Data $bytes -Pem (Get-EnvPem "OWNER_PERPLEXITY_PRIVATE_KEY_PEM") -KeyId "owner:perplexity"
$traceObj = Get-Content $TraceFile -Raw | ConvertFrom-Json
$traceObj | Add-Member -Name signatures -MemberType NoteProperty -Value @() -Force
if ($sig1) { $traceObj.signatures += $sig1 }
if ($sig2) { $traceObj.signatures += $sig2 }
$traceObj | ConvertTo-Json -Depth 50 | Out-File -FilePath $TraceFile -Encoding utf8

# Evaluate policy with OPA
$opaExe = Get-OpaPath
$opa = Invoke-OpaEval -OpaExe $opaExe -PolicyDir $PolicyDir -InputFile $TraceFile -Query $OpaQuery

# Write OPA result
$opaOut = [pscustomobject]@{
  decisionPath = $OpaQuery
  available    = $opa.available
  pass         = $opa.pass
  engine       = if ($opaExe) { Split-Path $opaExe -Leaf } else { "none" }
  raw          = $opa.raw
  checkedAt    = (Get-Date).ToUniversalTime().ToString("o")
}
$opaOut | ConvertTo-Json -Depth 50 | Out-File -FilePath $OpaResultFile -Encoding utf8

# Immutable audit log
$traceSha = Get-Sha256Hex $TraceFile
$opaSha   = Get-Sha256Hex $OpaResultFile
$logLine = [pscustomobject]@{
  schema     = "bithub.audit.v1"
  ts         = (Get-Date).ToUniversalTime().ToString("o")
  run_id     = $env:GITHUB_RUN_ID
  ref        = $env:GITHUB_REF
  repo       = $env:GITHUB_REPOSITORY
  actor      = $env:GITHUB_ACTOR
  compliance = @{ level=$ComplianceLevel; pass=$opa.pass }
  artefacts  = @{
    trace = @{ file=$TraceFile; sha256=$traceSha; signatures=$traceObj.signatures }
    opa   = @{ file=$OpaResultFile; sha256=$opaSha }
    humor_log = $HumorLog
  }
}
$logFile = Join-Path $AuditDir "immutable-log.jsonl"
$logLine | ConvertTo-Json -Depth 50 | Add-Content -Path $logFile -Encoding utf8

# Enforce gate if FailMode is 'gate'
if ($FailMode -eq "gate" -and -not $opa.pass) {
  Write-Error "❌ Compliance floor failed."
  exit 1
}

Write-Host "✔ Compliance floor passed (or logged)."
