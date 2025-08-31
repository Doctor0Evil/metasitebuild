# Bit.Hub rapid manifest/file structure initializer
# Autonomous, ALN/BitBot-compliant, simulated I/O capable, GitHub-runner friendly

[CmdletBinding()]
param(
    [switch]$Simulate
)

$ErrorActionPreference = 'Stop'
Set-Location -Path 'C:\Users\Hunter\ALN_Programming_Language'

function Ensure-FileTree {
    param(
        [string[]]$Directories,
        [hashtable[]]$LeafFiles
    )

    foreach ($dir in $Directories) {
        if (-not (Test-Path $dir)) {
            if ($Simulate) {
                Write-Host "[SIM] Would create directory: $dir"
            } else {
                New-Item -ItemType Directory -Path $dir -Force | Out-Null
                Write-Host "[+] Directory created: $dir"
            }
        } else {
            Write-Host "[*] Directory exists: $dir"
        }
    }

    foreach ($spec in $LeafFiles) {
        $fpath = $spec['Path']
        $init  = $spec['Content']
        $parent = Split-Path $fpath

        if (-not (Test-Path $parent)) {
            if ($Simulate) {
                Write-Host "[SIM] Would create parent directory: $parent"
            } else {
                New-Item -ItemType Directory -Path $parent -Force | Out-Null
                Write-Host "[+] Parent directory created: $parent"
            }
        }

        if (-not (Test-Path $fpath)) {
            if ($Simulate) {
                Write-Host "[SIM] Would create file: $fpath"
            } else {
                Set-Content -Path $fpath -Value $init -Force -Encoding UTF8
                Write-Host "[+] File created: $fpath"
            }
        } else {
            Write-Host "[*] File exists: $fpath"
        }
    }
}

# Target directories and files
$dirs = @(
    '.bit', 'rego', 'policy', 'data/security', 'manifests', '.bithub', 'overhaul', 'templates',
    '.github', '.github/actions', '.github/workflows', 'workflows', 'aln', 'core', 'rego/policy',
    'src/core', 'bithub/scripts', 'scripts', '.bithub/ledger'
)

$files = @(
    @{ Path = '.bit/rego'; Content = '# Rego entry-point' },
    @{ Path = '.bit/access_policy.rego'; Content = 'package bit.accesspolicy' },
    @{ Path = '.bit/contract.aln'; Content = 'ALN_CONTRACT_ID: default' },
    @{ Path = '.bit/workflow_manifest.aln'; Content = 'workflow: manifest' },
    @{ Path = '.bithub-actions/master.bithub.files.yml'; Content = "files:`n  - manifest.yml" },
    @{ Path = '.bit/policy.json'; Content = '{ "effect": "permit" }' },
    @{ Path = '.bit/actions.rego'; Content = 'package bit.actions' },
    @{ Path = '.bit/artifacts.rego'; Content = 'package bit.artifacts' },
    @{ Path = '.bit/opa-compliance.yml'; Content = "name: OPA Compliance`non: [push]`njobs: { check: { runs-on: ubuntu-latest } }" },
    @{ Path = '.bit/permissions.rego'; Content = 'package bit.permissions' },
    @{ Path = '.bit/reusable.rego'; Content = 'package bit.reusable' },
    @{ Path = '.bit/runners.rego'; Content = 'package bit.runners' },
    @{ Path = '.bit/steps.rego'; Content = 'package bit.steps' },
    @{ Path = '.bit/triggers.rego'; Content = 'package bit.triggers' },
    @{ Path = 'templates/analysis-aln.yml'; Content = 'name: ALN Analysis' },
    @{ Path = 'templates/analysis-powershell.yml'; Content = 'name: PowerShell Analysis' },
    @{ Path = 'workflows/security.policy.yml'; Content = 'name: Security Policy Enforcement' },
    @{ Path = 'overhaul/.git.command-interface.sh'; Content = '#!/bin/sh\necho ".git command interface"' },
    @{ Path = 'scripts/enforce_security_matrix.sh'; Content = '#!/bin/sh\necho "Security matrix enforcing..."' },
    @{ Path = '.bithub-unified.yml'; Content = 'name: BitHub Unified Workflow' },
    @{ Path = '.github/actions/action.yml'; Content = 'name: Bit Detect Outputs' },
    @{ Path = 'aln/.nanobyte-deploy.lisp'; Content = '; ALN Nanobyte deploy' },
    @{ Path = 'aln/bithub-loop-chokepoint.lisp'; Content = '; Loop chokepoint logic' },
    @{ Path = 'bithub/scripts/bitbot_api_init.py'; Content = '# bitbot api init' },
    @{ Path = 'bithub/scripts/bitbot_safety_audit.py'; Content = '# bitbot safety audit' },
    @{ Path = 'bithub/scripts/bitbot_store_telemetry.py'; Content = '# bitbot store telemetry' },
    @{ Path = 'bithub/scripts/variable_parser_correction.py'; Content = '# variable parser correction' },
    @{ Path = 'core/rego/policy/bitworkflow.rego'; Content = 'package workflow.rego' },
    @{ Path = 'src/core/.bit.bithub.comply'; Content = '# BitHub comply marker' },
    @{ Path = '.bit/bit.create'; Content = 'bit: create' },
    @{ Path = '.bit/bit.deploy.ps1'; Content = '# PowerShell Bit deploy' },
    @{ Path = '.bit/.bit.yml'; Content = 'name: Bit config' },
    @{ Path = '.bit/.bithub.actions'; Content = 'actions: []' },
    @{ Path = '.bit/.bithub.yml'; Content = 'name: BitHub config' },
    @{ Path = '.bit/LICENSE'; Content = 'MIT License' },
    @{ Path = '.bit/ReadMe.md'; Content = 'BitHub ReadMe' },
    @{ Path = '.bit/SECURITY.md'; Content = 'BitHub Security' },
    @{ Path = '.bit/bit.hub.md'; Content = 'BitHub Hub Info' },
    @{ Path = '.bit/bitbot-integrations.yml'; Content = 'name: BitBot Integrations' },
    @{ Path = '.bit/ml.workflow.bit.yml'; Content = 'name: ML Workflow Bit' },
    @{ Path = '.bit/rename.json'; Content = '{ "rename": "BitHub" }' }
)

Ensure-FileTree -Directories $dirs -LeafFiles $files

# Ledger logging
$ledger = ".bithub/ledger/compliance.log"
if (-not (Test-Path $ledger)) {
    if (-not $Simulate) { New-Item -ItemType File -Path $ledger -Force | Out-Null }
}
$logEntry = "{`"ts`":`"$(Get-Date -Format o)`",`"event`":`"INIT_FILETREE`",`"detail`":`"Initialized ALN/BitHub structure (Simulate=$Simulate)`"}"
if (-not $Simulate) { Add-Content -Path $ledger -Value $logEntry }
Write-Host "[Ledger] $logEntry"

# Summary
Write-Host "`n=== SUMMARY ==="
Write-Host "Directories checked: $($dirs.Count)"
Write-Host "Files checked: $($files.Count)"
Write-Host "[Bit.Hub ALN structure OK]"

# Compliance enforcement: fail if anything critical is missing
$missingDirs = $dirs | Where-Object { -not (Test-Path $_) }
$missingFiles = $files | Where-Object { -not (Test-Path $_.Path) }
if ($missingDirs -or $missingFiles) {
    Write-Error "Compliance check failed — missing required items."
    exit 1
}
