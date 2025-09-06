#!/usr/bin/env pwsh
Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

$metaPath = "bitshell.meta.json"
$meta = Get-Content $metaPath -Raw | ConvertFrom-Json

$logDir = ".bitshell/logs"
$artDir = ".bitshell/artifacts"
New-Item -ItemType Directory -Force -Path $logDir, $artDir | Out-Null

function Log { param([string]$m) $ts = (Get-Date).ToString("o"); "$ts $m" | Tee-Object -FilePath "$logDir/bitshell.log" -Append }

Log "BitShell(Functions): preflight"
$required = @("pwsh","docker","conftest")
foreach ($c in $required) { if (-not (Get-Command $c -ErrorAction SilentlyContinue)) { throw "Missing $c" } }

# Egress default
$blocked = [bool]$meta.trust.blocked_network
$dockerNet = if ($blocked) { "--network=none" } else { "" }

# Policy checks
Log "Policy checks via Conftest"
conftest test --policy $meta.policy.opa_policies_dir . | Tee-Object -FilePath "$logDir/policy.log" -Append

# Static analysis
Log "PSScriptAnalyzer"
if (-not (Get-Module -ListAvailable -Name PSScriptAnalyzer)) { Install-Module PSScriptAnalyzer -Scope CurrentUser -Force | Out-Null }
$psa = Invoke-ScriptAnalyzer -Path $meta.build.powershell.functions_dir -Recurse -Severity Error,Warning
if ($psa) {
  $errors = $psa | Where-Object { $_.Severity -eq "Error" }
  $warnings = $psa | Where-Object { $_.Severity -eq "Warning" }
  $psa | ConvertTo-Json -Depth 6 | Out-File "$logDir/psscriptanalyzer.json"
  if ($meta.build.powershell.treat_warnings_as_errors -and $warnings) { throw "PSScriptAnalyzer warnings present and treated as errors" }
  if ($errors) { throw "PSScriptAnalyzer errors present" }
}

# Unit tests with Pester (if tests present)
if (Test-Path "Tests") {
  Log "Pester tests"
  if (-not (Get-Module -ListAvailable -Name Pester)) { Install-Module Pester -Scope CurrentUser -Force | Out-Null }
  Invoke-Pester -Path "Tests" -CI -Output Detailed | Tee-Object -FilePath "$logDir/pester.log" -Append
} else {
  Log "No Tests/ directory; skipping Pester"
}

# Pin and restore modules
$req = $meta.build.powershell.pinned_modules_file
if (-not (Test-Path $req)) { throw "Pinned modules file not found: $req" }
Log "Restoring pinned modules from $req"
# Use a temp PSModulePath under .bitshell to keep deterministic restore
$env:PSModulePath = "$PWD\.bitshell\psmodules"
New-Item -ItemType Directory -Force -Path $env:PSModulePath | Out-Null
Save-Module -Path $env:PSModulePath -RequiredModules (Import-PowerShellDataFile $req).Modules -ErrorAction Stop

# Create deterministic package (zip)
$zipOut = $meta.build.package.output_zip
$zipDir = Split-Path -Parent $zipOut
New-Item -ItemType Directory -Force -Path $zipDir | Out-Null

Log "Packaging Function App"
# Normalize timestamps to 2000-01-01 for deterministic zips
Get-ChildItem -Recurse | ForEach-Object {
  if (-not $_.PSIsContainer) { $_.LastWriteTime = Get-Date "2000-01-01" }
}

Add-Type -AssemblyName System.IO.Compression.FileSystem
if (Test-Path $zipOut) { Remove-Item $zipOut -Force }
[System.IO.Compression.ZipFile]::CreateFromDirectory($meta.build.powershell.functions_dir, $zipOut)

$sizeMB = ((Get-Item $zipOut).Length / 1MB)
if ($sizeMB -gt [double]$meta.build.package.max_size_mb) { throw "Package exceeds max size $($meta.build.package.max_size_mb) MB: $sizeMB MB" }

# SBOM
Log "Generating SBOM"
$sbom = ".bitshell/artifacts/sbom.spdx.json"
# Syft over the working directory; optional: focus on modules directory
syft "dir:." -o spdx-json > $sbom

# Sign
Log "Signing artifacts with cosign"
if (-not (Get-Command "cosign" -ErrorAction SilentlyContinue)) { throw "cosign not installed" }
$cosignKey = $env:COSIGN_KEY
if (-not $cosignKey) { throw "COSIGN_KEY not set" }
& cosign sign-blob --yes --key "$cosignKey" --output-signature "$zipOut.sig" "$zipOut"

# Provenance (placeholder command; integrate your generator)
Log "Provenance"
"{}" | Out-File ".bitshell/artifacts/provenance.intoto.jsonl"

# Emit deployment plan (no execution)
$plan = @{
  subscription = $meta.trust.azure.subscription_id
  tenant = $meta.trust.azure.tenant_id
  resourceGroup = $meta.trust.azure.resource_group
  functionApp = $meta.trust.azure.function_app
  slot = $meta.trust.azure.slot
  package = $zipOut
  method = $meta.deploy.method
} | ConvertTo-Json -Depth 6
$plan | Out-File ".bitshell/artifacts/deploy-plan.json"

Log "BitShell(Functions): complete"
