### Bit.Hub.runner mesh protocol

You want runners that feel alive: fast, adaptive, and fun. Here’s a compact, enforceable mesh protocol that lets Bit.Hub.runners discover each other, advertise capabilities, negotiate jobs, enforce compliance with OPA, move artifacts across storage backends, and light up ALNFantasia’s creative modules.

---

## Protocol overview

- **Transport**
  - **LAN**: mDNS + HTTP(S)/WebSocket control channel.
  - **P2P**: TCP/WebSocket with secure handshakes; optional IPFS/libp2p gateway for artifact routing.
  - **Fallback**: Static seed list for isolated networks.

- **Contracts**
  - **Runner manifest**: Declares identity, capabilities, policies, and storage drivers.
  - **Mesh config**: Discovery settings, seeds, QoS, and ledger sinks.
  - **Message schema**: Versioned JSON for hello, heartbeat, job offer/claim, artifact ops, policy queries, and events.

- **Governance**
  - **OPA Rego** in every runner: all decisions are policy-gated.
  - **Ledger**: append-only, hash-chained logs under .bithub/ledger for audit.

- **Fun layer**
  - Event hooks broadcast to ALNFantasia modules: creative.enjoyments.exe, fan.asia.create, magic.lol.

---

## Identity, security, and compliance

#### Runner manifest (.bit/runner.manifest.yml)
```yaml
runner:
  id: "aln://runner/phoenix/alpha"
  version: "1.0.0"
  persona: "hybrid"  # strict|creative|hybrid
  capabilities:
    build: ["node18","python3.11","docker"]
    test: ["pytest","jest"]
    ml: ["train","evaluate"]
    entertainment: ["creative.enjoyments.exe","magic.lol","fan.asia.create"]
  storage:
    default: "ns"
    drivers:
      ns: { scheme: "aln+ns://", roots: [".bithub/storage",".git-lfs"], tamper_evident: true }
      s3: { scheme: "aln+s3://", bucket: "bit-hub-artifacts", region: "us-west-2" }
      ipfs: { scheme: "ipfs://", gateway: "https://ipfs.io" }
  policies:
    dir: ".bithub/policies"
    enforce_strict: true
  ledger:
    path: ".bithub/ledger/runner.log"
  qos:
    max_concurrent_jobs: 3
    priority_bias: ["compliance","latency","fun"]
```

#### Mesh config (.bit/mesh/config.yml)
```yaml
mesh:
  version: "1.0.0"
  discovery:
    mdns: true
    p2p: true
    seeds:
      - "wss://seed1.bithub.mesh"
      - "wss://seed2.bithub.mesh"
  tls:
    required: true
    mode: "self-signed-ok"  # self-signed-ok|strict-ca
  auth:
    jwt_issuer: "aln://bithub/auth"
    accept_personas: ["strict","hybrid","creative"]
  rate_limits:
    per_peer_rps: 10
    burst: 50
```

#### Hash-chained ledger entry (append to .bithub/ledger/runner.log)
```json
{"ts":"2025-08-30T16:11:02Z","actor":"aln://runner/phoenix/alpha","event":"JOB_CLAIM","job_id":"J-92A7","prev_sha256":"<prev>","payload_sha256":"<sha>","entry_sha256":"<sha>"}
```

#### OPA guard (example) (.bithub/policies/mesh.rego)
```rego
package bithub.mesh

default allow_job = false

allow_job {
  input.msg.type == "JOB_OFFER"
  input.msg.payload.required.capabilities[_] == "build"
  not forbidden_repo(input.msg.payload.repo)
  input.msg.payload.priority <= input.config.max_priority
}

forbid_persona[msg] {
  not input.peer.persona == "strict"
  input.msg.payload.required.compliance == "critical"
  msg := "critical jobs require strict persona"
}

forbidden_repo(repo) {
  endswith(repo, "/forbidden")
}
```

---

## Discovery and registration

#### LAN discovery (mDNS record)
- Service: _bithub-runner._tcp.local
- TXT keys:
  - id=aln://runner/phoenix/alpha
  - v=1.0.0
  - persona=hybrid
  - caps=build,test,ml,entertainment
  - addr=wss://host.local:7443

#### Registration handshake (HELLO/ACK)
- Client → Mesh: HELLO
- Mesh → Client: HELLO_ACK (with session token)

Example HELLO
```json
{
  "type": "HELLO",
  "schema": "bithub.mesh.hello.v1",
  "runner_id": "aln://runner/phoenix/alpha",
  "persona": "hybrid",
  "capabilities": {"build":["node18"],"ml":["train"],"entertainment":["magic.lol"]},
  "storage": ["aln+ns://",".git-lfs","ipfs://"],
  "policy_hash": "sha256-2b6f...",
  "public_key": "ed25519:8abf..."
}
```

Example HELLO_ACK
```json
{
  "type": "HELLO_ACK",
  "schema": "bithub.mesh.hello_ack.v1",
  "session": "jwt-token-here",
  "mesh_time": "2025-08-30T16:11:38Z",
  "upgrade_hint": null
}
```

Heartbeats every 10–30s:
```json
{"type":"HEARTBEAT","schema":"bithub.mesh.heartbeat.v1","runner_id":"...","jobs_active":1,"load":0.42}
```

---

## Messaging and schemas

All messages are JSON over WebSocket/TLS. Each has:
- type, schema, msg_id, sender, ts, payload

#### JOB_OFFER
```json
{
  "type": "JOB_OFFER",
  "schema": "bithub.mesh.job.offer.v1",
  "msg_id": "M-1",
  "sender": "aln://mesh/controller",
  "ts": "2025-08-30T16:12:00Z",
  "payload": {
    "job_id": "J-92A7",
    "repo": "git+https://example.com/foo/bar.git#main",
    "required": {
      "capabilities": ["build"],
      "policies": ["bithub.repo.structure","bithub.workflows.ledger"],
      "persona": "strict"
    },
    "artifacts_in": ["aln+ns://datasets/train"],
    "artifacts_out": ["aln+ns://builds/bar/{job_id}.tar.gz"],
    "priority": 7,
    "timeout_s": 1800
  }
}
```

#### JOB_BID (optional competitive claim)
```json
{
  "type": "JOB_BID",
  "schema": "bithub.mesh.job.bid.v1",
  "msg_id": "M-2",
  "sender": "aln://runner/phoenix/alpha",
  "ts": "2025-08-30T16:12:02Z",
  "payload": {
    "job_id": "J-92A7",
    "eta_s": 420,
    "cost_units": 5,
    "confidence": 0.92
  }
}
```

#### JOB_CLAIM / JOB_ASSIGN
```json
{"type":"JOB_CLAIM","schema":"bithub.mesh.job.claim.v1","payload":{"job_id":"J-92A7"}}
{"type":"JOB_ASSIGN","schema":"bithub.mesh.job.assign.v1","payload":{"job_id":"J-92A7","runner_id":"..."}}
```

#### ARTIFACT_PUT / ARTIFACT_GET
```json
{"type":"ARTIFACT_PUT","schema":"bithub.mesh.artifact.put.v1","payload":{"uri":"aln+ns://builds/bar/J-92A7.tar.gz","sha256":"...","size":123456}}
{"type":"ARTIFACT_GET","schema":"bithub.mesh.artifact.get.v1","payload":{"uri":"ipfs://bafy..."}}
```

#### POLICY_QUERY (OPA ask)
```json
{
  "type": "POLICY_QUERY",
  "schema": "bithub.mesh.policy.query.v1",
  "payload": {
    "package": "bithub.mesh",
    "rule": "allow_job",
    "input": { "msg": { "type": "JOB_OFFER", "payload": { "required": {"capabilities":["build"],"compliance":"critical"} } }, "peer": {"persona":"strict"}, "config": {"max_priority": 10} }
  }
}
```

#### EVENT_EMIT (creative + fun)
```json
{
  "type": "EVENT_EMIT",
  "schema": "bithub.mesh.event.emit.v1",
  "payload": {
    "source": "aln://runner/phoenix/alpha",
    "event": "magic.lol",
    "tags": ["success","celebration","ascii-dragon"],
    "context": {"job_id":"J-92A7","mood":"radiant"}
  }
}
```

---

## Job lifecycle and scheduling

1. **Offer**: Mesh emits JOB_OFFER to candidate runners (capability/persona filter).
2. **Policy gate**: Runner asks OPA allow_job; denies fast if not allowed.
3. **Bid**: Runner optionally replies with JOB_BID (ETA/cost/confidence).
4. **Assign**: Mesh selects runner (policy + score) and sends JOB_ASSIGN.
5. **Execute**:
   - Checkout repo (read-only token).
   - Run preflight OPA suite.
   - Execute task; stream logs; write ledger entries.
   - Store artifacts via configured storage driver.
6. **Complete**: Runner emits JOB_RESULT with status, metrics, artifact URIs.
7. **Celebrate**: On success, emit EVENT_EMIT for creative.enjoyments.exe, magic.lol, fan.asia.create.

Scoring hint (runner-local):
```ts
function score(job, runner) {
  const cap = jaccard(job.required.capabilities, runner.capabilities);
  const load = 1 - runner.load;
  const persona = job.required.persona === runner.persona ? 1 : 0.5;
  return 0.6*cap + 0.3*load + 0.1*persona;
}
```

---

## Storage, artifacts, and entertainment modules

#### Uniform URIs
- aln+ns://local-root/path
- aln+s3://bucket/prefix/key
- ipfs://CID
- file:// for dev-only

#### Content addressing & chunking
- Compute sha256 for every artifact; include size and chunks metadata in ledger.
```json
{"event":"ARTIFACT_COMMIT","uri":"aln+ns://builds/bar/J-92A7.tar.gz","sha256":"...","size":123456,"chunks":4}
```

#### Minimal driver interfaces (.bit/sdk/types.ts)
```ts
export interface StorageDriver {
  scheme(): string;
  read(uri: string): Promise<Uint8Array>;
  write(uri: string, bytes: Uint8Array, meta?: Record<string, unknown>): Promise<void>;
  stat(uri: string): Promise<{ size: number; sha256?: string }>;
  list(prefix: string): Promise<string[]>;
}
export interface LedgerDriver { append(event: Record<string, unknown>): Promise<void>; verify(): Promise<{ ok: boolean; head: string }>; }
```

#### Entertainment hooks (runner-local)
- After JOB_RESULT success:
  - Write EVENT_EMIT with tags based on job metrics.
  - If enabled, call local module handlers:
    - magic.lol → ascii celebration in logs.
    - creative.enjoyments.exe → generate short lore card into .bithub/ledger/lore.log.
    - fan.asia.create → send world-event cue to ALNFantasia.

Example hook (.bit/hooks/success.sh)
```bash
#!/usr/bin/env bash
set -euo pipefail
JOB_ID="$1"; MOOD="${2:-radiant}"
echo "[magic.lol] (>*_*)>~~~ DRAGON ROAR ~~~<(*_*<)"
echo "{\"event\":\"lore\",\"job\":\"$JOB_ID\",\"mood\":\"$MOOD\"}" >> .bithub/ledger/lore.log
```

Make executable:
```bash
chmod +x .bit/hooks/success.sh
```

---

## Quick start runner and CI wiring

#### Repo layout
```
/
├─ .bit/
│  ├─ runner.manifest.yml
│  ├─ mesh/config.yml
│  ├─ sdk/...
│  ├─ hooks/success.sh
│  └─ loaders/runner_boot.sh
├─ .bithub/
│  ├─ policies/*.rego
│  └─ ledger/
└─ .github/workflows/bit-hub-runner.yml
```

#### Runner boot script (.bit/loaders/runner_boot.sh)
```bash
#!/usr/bin/env bash
set -euo pipefail
MANIFEST=".bit/runner.manifest.yml"
CONFIG=".bit/mesh/config.yml"
LEDGER=$(yq '.runner.ledger.path' "$MANIFEST")

mkdir -p "$(dirname "$LEDGER")"
echo "[boot] $(date -Iseconds) starting $(yq '.runner.id' "$MANIFEST")" >> "$LEDGER"

# Start OPA (policies) in background
opa run --server --addr :8181 "$(yq '.runner.policies.dir' "$MANIFEST")" &

# Discover/register (stubbed; replace with your mesh daemon)
echo "[mesh] mdns=$(yq '.mesh.discovery.mdns' "$CONFIG") p2p=$(yq '.mesh.discovery.p2p' "$CONFIG")"

# Heartbeat loop
while true; do
  echo "{\"ts\":\"$(date -Iseconds)\",\"event\":\"HEARTBEAT\",\"load\":0.12}" >> "$LEDGER"
  sleep 15
done
```

#### GitHub Action to spawn a local mesh node (.github/workflows/bit-hub-runner.yml)
```yaml
name: Bit.Hub Runner Node

on:
  workflow_dispatch:

jobs:
  node:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Install deps
        run: |
          sudo apt-get update && sudo apt-get install -y yq jq
          curl -L -o opa https://openpolicyagent.org/downloads/latest/opa_linux_amd64_static
          chmod +x opa && sudo mv opa /usr/local/bin/opa
      - name: Boot runner (demo)
        run: nohup .bit/loaders/runner_boot.sh >/dev/null 2>&1 & echo "Runner booted"
```

---

If you want, I can extend this with:
- A minimal mesh controller that implements JOB_OFFER → BID → ASSIGN.
- An ns storage driver and IPFS adapter.
- A conftest-powered compliance gate wired to evolutionous.loop.aln, so jobs can’t start unless the loop and decompression policies pass.

Say the word, and we’ll make your mesh feel like a living MMO for builds.
