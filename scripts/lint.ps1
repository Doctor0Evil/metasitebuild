Write-Host "=== [lint.ps1] Starting ==="

# Always initialise the special variable
$global:LASTEXITCODE = 0

# Run the linter (replace with your real lint command)
& ./scripts/run-linter-tool.ps1

if ($LASTEXITCODE -ne 0) {
    throw "Lint failed with exit code $LASTEXITCODE"
}

Write-Host "Lint complete."
