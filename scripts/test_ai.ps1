Write-Host "=== [test_ai.ps1] Starting ==="

$global:LASTEXITCODE = 0

# Run the AI tests
& ./scripts/run-ai-tests.ps1

if ($LASTEXITCODE -ne 0) {
    throw "AI tests failed with exit code $LASTEXITCODE"
}

Write-Host "AI module tests passed."
