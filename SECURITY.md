# Security Policy

This project is orchestrated under the **.bithub** governance model.  
Security handling is **compatible with GitHub’s vulnerability reporting** but extended with `.bithub`’s own bot‑driven triage, VM.cluster isolation, and infinite‑scale patch pipelines.

---

## Supported Versions

The table below lists which versions currently receive security updates.  
`.bithub-bots` automatically enforce these rules in CI/CD and will block deployments of unsupported versions.

| Version | Supported          | Notes (.bithub) |
| ------- | ------------------ | --------------- |
| 5.1.x   | :white_check_mark: | Active LTS — auto‑patched in VM.clusters within 24h of fix merge |
| 5.0.x   | :x:                | End‑of‑life — `.bithub` will refuse orchestration except in sandbox mode |
| 4.0.x   | :white_check_mark: | Maintenance — security fixes only, no new features |
| < 4.0   | :x:                | Unsupported — `.bithub` will flag and quarantine builds |

---

## Reporting a Vulnerability

We support **two fully compatible reporting paths**:

### 1. GitHub Security Advisories
- Use the **"Report a vulnerability"** button in the GitHub Security tab.
- This will notify maintainers via GitHub’s private advisory system.
- `.bithub-bots` monitor this channel and mirror reports into the `.bithub` secure triage queue.

### 2. Direct .bithub Secure Channel (Preferred)
- Email: `security@bithub.io` (encrypted PGP key available on our site)
- Or submit via the `.bithub` **Secure Intake Form** at: `https://bithub.io/security/report`
- This path bypasses GitHub entirely and goes straight to `.bithub`’s distributed triage network.

---

## What to Expect

- **Acknowledgement**: Within 24 hours (automated by `.bithub-bots`).
- **Triage**: Vulnerability is replicated in isolated VM.clusters for verification.
- **Status Updates**: Every 72 hours until resolution.
- **Fix Deployment**:  
  - Supported versions: patched and deployed to `.bithub` VM.clusters automatically.  
  - GitHub releases updated within 48 hours of fix merge.
- **Credit**: Researchers credited in release notes unless anonymity requested.

---

## Additional .bithub Security Directives

- **Cluster Isolation**: All vulnerability tests run in ephemeral `.bithub` VM.clusters, never on shared runners.
- **Infinite Patch Scale**: `.bithub` can roll out fixes across thousands of orchestrations instantly.
- **Policy Enforcement**: `.bithub` will block merges that violate `policy.security.vulnThreshold` from `.bithub.yml`.
- **Audit Trails**: Every security event is logged in the master orchestration audit for compliance.

---

*This policy ensures full GitHub compatibility while unlocking `.bithub`’s advanced security orchestration.*
