Write-Host "=== [combat-sim.ps1] Starting ==="

$global:LASTEXITCODE = 0

# Run the combat simulation
& ./scripts/run-combat-sim.ps1

if ($LASTEXITCODE -ne 0) {
    throw "Combat simulation failed with exit code $LASTEXITCODE"
}

Write-Host "Combat sim complete."
