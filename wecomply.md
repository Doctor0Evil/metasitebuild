# manifest.delta.yml
apiVersion: bit.hub/v1
kind: Update
metadata:
  federationId: "82jfw4.bit"
spec:
  policies:
    - "policies/fetch-agents.yaml"          # Capabilities, spend caps, counterparty rules
    - "policies/fl-robust-agg.yaml"         # Median/Krum/trimmed-mean + consensus gates
    - "policies/tx-evidence.yaml"           # Evidence schema, retention, redaction
  registries:
    agents: "mesh://registry.bithub/fetch-agents"
  workflows:
    - name: "fetch-agent-register"
      triggers:
        - manual: ["governance"]
      steps:
        - name: "provision-keys"
          action: "kms/generate"
          with: { scope: "agent", attest: true }
        - name: "build-and-sign"
          action: "build/image"
          with: { sbom: true, sign: true, slsa: "L3" }
        - name: "anchor-identity"
          action: "agents/register"
          with: { registry: "mesh://registry.bithub/fetch-agents" }
        - name: "celebrate"
          action: "bots/celebrate"
          with: { theme: "agent-born" }

    - name: "fetch-agent-run"
      triggers:
        - schedule: "cron: '*/5 * * * *'"
      steps:
        - name: "pull-policy"
          action: "governance/sync"
        - name: "start-agent"
          action: "containers/run"
          with:
            image: "agents/fetch/uagent:stable"
            envFrom: "sealed://secrets/agent"
            limits:
              spendPerTx: "50"
              spendPerDay: "500"
        - name: "telemetry"
          action: "agents/emit"
          with: { topics: ["tx","message","error"] }

    - name: "fetch-tx-monitor"
      triggers:
        - events: ["agent.tx","agent.message"]
      steps:
        - name: "normalize"
          action: "tx/normalize"
          with: { schema: "schemas/fetch.tx.v1.json" }
        - name: "evidence-freeze"
          action: "evidence/freeze"
          with: { integrity: "sha256+timestamper" }
        - name: "counterparty-check"
          action: "risk/screen"
          with: { lists: ["deny","allow"], anomalyThreshold: 2.0 }
        - name: "ic3-labels"
          action: "incidents/label"
          with: { rubric: "policies/ic3-reporting.yaml" }
        - name: "export-ic3"
          if: "labels.ic3_triggered == true"
          action: "compliance/export"
          with: { format: ["html","pdf","json-index"] }
        - name: "celebrate"
          if: "labels.ic3_triggered == false"
          action: "bots/celebrate"
          with: { theme: "clean-ledger" }
      rescue:
        - name: "rescue-pack"
          action: "compliance/rescue-package"
          alwaysSucceed: true

    - name: "fl-from-fetch"
      schedule: "cron: '*/20 * * * *'"
      steps:
        - name: "gather-updates"
          action: "fl/collect"
          with:
            source: "agents/fetch/*"
            dp:
              epsilon: 4.0
              maxGradNorm: 1.0
        - name: "aggregate-robust"
          action: "fl/aggregate"
          with:
            method: ["median","krum","trimmed-mean"]
            consensus: "2of3"
            anomalyThreshold: 2.5
        - name: "shadow-validate"
          action: "models/eval"
          with:
            suites: ["regression","backdoor-canaries"]
            maxTriggerShift: 0.5
        - name: "promote"
          if: "checks.ok && consensus.ok"
          action: "models/promote"
          with: { sign: true, rollout: "blue-green" }
        - name: "rollback"
          if: "checks.ok == false || consensus.ok == false"
          action: "models/rollback"
          alwaysSucceed: true

    - name: "drift-guardian"
      schedule: "cron: '*/10 * * * *'"
      steps:
        - name: "diff-policies"
          action: "governance/diff"
        - name: "auto-repair"
          action: "repair/apply"
          with: { allowed: ["policy","config","labels"] }
        - name: "rename-cycle"
          action: "continuity/rename"
          with: { strategy: "blue-green" }
        - name: "audit"
          action: "audit/log"
          with: { type: "drift.healed" }
      rescue:
        - name: "freeze-and-notify"
          action: "governance/freeze"
          with: { scope: "affected-namespaces" }
          alwaysSucceed: true
