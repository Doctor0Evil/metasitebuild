Write-Host ">>> Starting lint.ps1"
try {
    & ./scripts/lint.ps1
} catch {
    Write-Error "lint.ps1: $_"
}

Write-Host ">>> Starting test_ai.ps1"
try {
    & ./scripts/test_ai.ps1
} catch {
    Write-Error "test_ai.ps1: $_"
}

Write-Host ">>> Starting combat-sim.ps1"
try {
    & ./scripts/combat-sim.ps1
} catch {
    Write-Error "combat-sim.ps1: $_"
}

# If any failures occurred, exit with code 1
if ($Error.Count -gt 0) {
    throw "One or more AI/NPC tests failed."
}

Write-Host "All AI/NPC tests passed."
