# hybrid.workflow.file
version: "1.0.0"
name: Bit.Hub-BitCommander-Model
description: >
  Central command model for overseeing security, compliance, and integrity checks across Bit.Hub, ALN, .bit.*, and CI runners.

corePrinciples:
  - Zero-Trust Integrity: All workflows/artifacts verified, nothing trusted by default.
  - Proactive Defense: Pre-execution security barriers on all user/app inputs.
  - Authority & Transparency: All gates and policies visible and strictly enforced.
  - Hardening Evolution: Policies/update cycles for adaptive compliance.

workflowOversight:
  - name: Great.Wall.of.Compliance
    path: .github/workflows/Great.Wall.of.Compliance.yml
    integrityCheck:
      enabled: true
      method: sha256
      expectedHash: 2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2d3e4f5a6b7c8d9e0f1a2b3c
      lockFile: .bit/workflow.lock
    required: true
    description: >
      Main compliance gate. Must exist; hash lock ensures untampered logic.
  - name: Malware.Scan
    path: .github/workflows/Malware.Scan.yml
    integrityCheck:
      enabled: true
      method: sha256
      expectedHash: a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2
      lockFile: .bit/workflow.lock
    required: true
    description: >
      Malware scanner. Blocks artifact/dependency threats.

securityPolicies:
  userPromptInjection:
    enabled: true
    detectionEngine: AI-Powered
    patternSources:
      - disallowed_terms.txt
      - central_injection_registry.json
    scanTargets:
      - user_input_prompts
      - commit_messages
      - comment_threads
    onDetection: quarantine
  assetEconomy:
    enforce: true
    requirements:
      portable: true
      blockchain: false
      trading:
        enabled: true
        requiresReview: true
  dependencyAuditing:
    enabled: true
    allowedRegistries:
      - https://registry.npmjs.org/
      - https://pypi.org/simple/
      - https://crates.io/
    enforceVulnerabilityScanning: true
  secretsManagement:
    enforce: true
    preventHardcodedSecrets: true
    minEntropy: 128

evolutionCycles:
  policyUpdates:
    cadence: daily
    source: https://internal.bithub.com/policies/latest
    verificationMethod: PGP
  modelUpdates:
    strategy: manual_review_and_approval
    auditors:
      - security_team
      - compliance_officer

complianceGate:
  blockingPolicy: block_merge
  quarantineDir: .bithubreports/quarantine
  reportFormat: json

ciIntegrations:
  - runnerType: github-hosted
    engines: [ ubuntu-latest, windows-latest, macos-latest ]
  - runnerType: self-hosted
    engines: [ aln, bit.bot, .bit.coin ]
  - botIntegration:
      allow: [ bit.bot, .bit.bots ]
      enforceIntegrity: true
  - artifactReports:
      baseDir: .bithubreports
      retentionDays: 14
      maskSensitive: true
