name: Run Humor Reasoning Model Compliance
on:
  workflow_dispatch:
  push:
    branches: [main, develop, earliest-critical]
  pull_request:
    branches: [main, develop]

jobs:
  humor-reasoning-compliance:
    name: Humor Reasoning Model + Compliance Floor
    runs-on: windows-latest
    concurrency:
      group: "Bit.Hub-${{ github.workflow }}-${{ github.ref }}"
      cancel-in-progress: false
    steps:
      - name: Checkout repository
        uses: actions/checkout@v4
        with:
          ref: ${{ github.sha }}

      - name: BitShell/ALN Hybrid: ALN+Pwsh Init
        shell: pwsh
        run: |
          Write-Host "::notice::Initializing BitShell ALN environment..."
          $Context = @{
            Workflow = "${{ github.workflow }}"
            Ref      = "${{ github.ref }}"
            RunId    = "${{ github.run_id }}"
            Actor    = "${{ github.actor }}"
            Repository = "${{ github.repository }}"
          }
          $Context | ConvertTo-Json -Depth 4 | Write-Host

      - name: Audit, Compliance & Sovereignty Check
        shell: pwsh
        env: 
          OWNER_BITHUB_PRIVATE_KEY_PEM: ${{ secrets.OWNER_BITHUB_PRIVATE_KEY_PEM }}
          OWNER_PERPLEXITY_PRIVATE_KEY_PEM: ${{ secrets.OWNER_PERPLEXITY_PRIVATE_KEY_PEM }}
        run: |
          $TraceFile = ".bithub/audit/humor-reasoning-trace.json"
          ./scripts/run-hrm-compliance.ps1 `
            -ComplianceLevel "strict" `
            -AuditDir ".bithub/audit" `
            -PolicyDir ".bithub/policies" `
            -HumorLog ".bithub/logs/humor-bot.log" `
            -TraceFile $TraceFile `
            -OpaResultFile ".bithub/audit/opa-result.json" `
            -OpaQuery "data.bithub.allow" `
            -FailMode "gate" `
            -AutoInstallOpa

          # Signature injection (Bit.Hub + Perplexity signoff)
          $traceJson = Get-Content $TraceFile -Raw | ConvertFrom-Json
          $traceJson | Add-Member -Name signatures -MemberType NoteProperty -Value @() -Force

          if ($env:OWNER_BITHUB_PRIVATE_KEY_PEM) {
            $sig1 = Sign-FileRsa -Path $TraceFile -Pem $env:OWNER_BITHUB_PRIVATE_KEY_PEM -KeyId "owner:bithub"
            if ($sig1) { $traceJson.signatures += $sig1 }
          }
          if ($env:OWNER_PERPLEXITY_PRIVATE_KEY_PEM) {
            $sig2 = Sign-FileRsa -Path $TraceFile -Pem $env:OWNER_PERPLEXITY_PRIVATE_KEY_PEM -KeyId "owner:perplexity"
            if ($sig2) { $traceJson.signatures += $sig2 }
          }
          $traceJson | ConvertTo-Json -Depth 20 | Out-File -FilePath $TraceFile -Encoding utf8

      - name: Audit Immutable Logging
        shell: pwsh
        run: |
          $traceSha = Get-FileHash ".bithub/audit/humor-reasoning-trace.json" -Algorithm SHA256
          $opaSha   = Get-FileHash ".bithub/audit/opa-result.json" -Algorithm SHA256
          $logEntry = [PSCustomObject]@{
            schema    = "bithub.audit.v1"
            ts        = (Get-Date).ToUniversalTime().ToString("o")
            run_id    = "${{ github.run_id }}"
            ref       = "${{ github.ref }}"
            repo      = "${{ github.repository }}"
            actor     = "${{ github.actor }}"
            artefacts = @{
              trace = @{ file = ".bithub/audit/humor-reasoning-trace.json"; sha256 = $traceSha.Hash }
              opa   = @{ file = ".bithub/audit/opa-result.json"; sha256 = $opaSha.Hash }
            }
          }
          $logFile = ".bithub/audit/immutable-log.jsonl"
          $logEntry | ConvertTo-Json -Depth 10 | Add-Content -Path $logFile -Encoding utf8
          Write-Host "::notice::Immutable audit log updated: $logFile"
