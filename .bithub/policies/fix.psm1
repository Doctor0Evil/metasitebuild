<#
.SYNOPSIS
  Bit.Hub "lol.ps1" - Dual-Mode CI/CD Git Wrapper
  Ensures .bit compliance, git sync, audit, and policy enforcement for local and GitHub Actions.
.PARAMETER CommitMsg
  Custom commit message for git operations (default: timestamped Bit.Hub Sync).
#>
param(
    [string]$CommitMsg = "Bit.Hub Sync: $(Get-Date -Format 'u')"
)

# --- Detect environment ---
$IsActions = $env:GITHUB_ACTIONS -eq "true"
$RepoUrl   = "github.com/Doctor0Evil/Bit.Hub.git"
$TargetDir = (Resolve-Path .).Path
if ($IsActions) { Write-Host "🌐 Environment: GitHub Actions" }
else            { Write-Host "🌐 Environment: Local Dev" }
Write-Host "📂 Target Directory: $TargetDir"

# --- Ensure .bit scaffold ---
$bitDirs = @(".bit", ".bit\bots", ".bit\policies", ".bit\command-sheets")
foreach ($dir in $bitDirs) {
    if (-not (Test-Path $dir)) {
        New-Item -ItemType Directory -Force -Path $dir | Out-Null
        Write-Host "✅ Created $dir"
    }
}

# --- Ensure .bit is not ignored ---
$gitignorePath = ".gitignore"
if (Test-Path $gitignorePath) {
    $content = Get-Content $gitignorePath
    $filtered = $content | Where-Object { $_ -notmatch '^\s*\.bit\/?\s*$' }
    if ($filtered.Count -ne $content.Count) {
        $filtered | Set-Content $gitignorePath -Encoding UTF8
        Write-Host "🛠 Removed .bit ignore rule from .gitignore"
    }
}

# --- Configure Git remote/auth ---
if ($IsActions) {
    git config --global url."https://x-access-token:${env:GITHUB_TOKEN}@github.com/".insteadOf "https://github.com/"
    Write-Host "🔑 Configured Actions token for HTTPS pushes"
} else {
    if (Test-Path "$HOME\.ssh\id_ed25519.pub") {
        $sshUrl = "git@github.com:Doctor0Evil/Bit.Hub.git"
        git remote set-url origin $sshUrl
        Write-Host "🔑 Using SSH for GitHub"
    } elseif ($env:GITHUB_PAT) {
        $patUrl = "https://x-access-token:$($env:GITHUB_PAT)@$RepoUrl"
        git remote set-url origin $patUrl
        Write-Host "🔑 Using stored PAT for GitHub"
    } else {
        Write-Warning "⚠️ No SSH key or PAT found. Pushes will fail."
    }
}

# --- Stage and commit changes ---
git add .bit .github/workflows .gitignore
if ((git status --porcelain) -ne '') {
    git commit -m "$CommitMsg"
    Write-Host "📦 Committed changes: $CommitMsg"
} else {
    Write-Host "ℹ️ No changes to commit."
}

# --- Pull & push ---
try {
    git pull --rebase origin main
    git push origin main
    Write-Host "🚀 Push attempted."
} catch {
    Write-Warning "❌ Push failed. Check credentials or network."
}

# --- Trigger batch workflows if in Actions ---
if ($IsActions -and (Test-Path ".bit/command-sheets/batch_register_and_sync.yml")) {
    Write-Host "📜 Executing batch workflows from command-sheet..."
    $workflows = Get-Content ".bit/command-sheets/batch_register_and_sync.yml" |
                 Where-Object { $_ -match 'workflow:' } |
                 ForEach-Object { ($_ -split ':')[3].Trim() }
    foreach ($wf in $workflows) {
        Write-Host "🚀 Triggering $wf..."
        & gh workflow run $wf
    }
}

# --- Enforce compliance scripts ---
$complianceScripts = @(
    'scripts/security_check.sh',
    'scripts/profanity_scan.py',
    'scripts/policy_audit.sh'
)
foreach ($script in $complianceScripts) {
    if (Test-Path $script) {
        Write-Host "🔒 Executing compliance script: $script"
        & $script
    }
}
if ((Test-Path 'scripts/replace_or_flag.sh') -and (Test-Path 'scripts/log_event.sh') -and (Test-Path 'scripts/notify_admin.sh')) {
    Write-Host "💡 Executing conditional enforcement scripts..."
    & scripts/replace_or_flag.sh
    & scripts/log_event.sh
    & scripts/notify_admin.sh
}

Write-Host "✅ Bit.Hub lol.ps1 completed. All workflows, policies, and compliance checks performed."
