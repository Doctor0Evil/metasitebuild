name: "Bit.Hub Compliance Wall: BitBot-Runner Floor"
on:
  push: {branches: [main, develop, bitcore, aln]}
  workflow_dispatch:
concurrency:
  group: Bit.Hub/${{ github.workflow }}-${{ github.ref }}
  cancel-in-progress: true
env:
  BITHUB_AUDIT_DIR: .bithub/audit
  BITLINKS_PATH: .bitlinks/bitlinks.aln
  HUMOR_LOG: .bithub/logs/humor-bot.log
  BITHUB_TRACE_FILE: .bithub/audit/humor-bot-trace.json

jobs:
  bitcompliance-gate:
    name: "BitBot Compliance Wall Enforcer"
    runs-on: [self-hosted, bitbot-secure-group]
    steps:
      - name: "BitShell/ALN Hybrid: ALN+Pwsh Init"
        shell: pwsh
        run: |
          Import-Module -Name ./.bit/BitHubCore.psm1 -Force
          Start-ALNCore -Strict
          ./.bit/bit.hub.ps1
      - name: "Audit, Compliance & Sovereignty Check"
        shell: pwsh
        run: |
          $Context = @{
            owner     = "Jacob Scott Farmer"
            team      = "Perplexity AI"
            signature = "BIT_HUB_WALL_UNBREAKABLE"
          }
          ./bithub/scripts/bitbot_safety_audit.py $Context
      - name: "Audit Immutable Logging"
        shell: pwsh
        run: |
          # Log everything with cryptographic hash, BitHub/ALN sig, and resistance stamp
          ./bithub/scripts/bitbot_store_telemetry.py
      - name: "Strict Policy + Humor-Reasoning-Core"
        shell: pwsh
        run: |
          ./bithub/scripts/bitbot_api_init.py --personality-core ./assets/data/personality_vectors/humor-core.json
      - name: "OPA/ALN/LOLCODE/Lisp Enforcement"
        shell: bash
        run: |
          opa eval --data .bit/compliance.rego --input .bithub/audit/last_submission.json --format=pretty && echo "OPA compliance pass"
          aln eval -f .bit/aln.compliance-figure && echo "ALN compliance pass"
          lisp -l alnfantasia.combat.profanity-keeper.lisp --execute
      - name: "ReadMe, Ownership, and License Enforcement"
        shell: pwsh
        run: |
          # Ensure contributors & credits are immutable
          ./scripts/enforce_security_matrix.sh
