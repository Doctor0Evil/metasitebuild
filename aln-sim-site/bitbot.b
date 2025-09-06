### Overview

Here’s a simulation‑only Jekyll integration for a “nanoswarm” panel, plus a nanoscale BitBot file named bithubbitbot.b (for AI/superintelligence)—all CSP‑clean, doc‑first, and wired to your existing metamanifest.

---

## File layout additions

```text
aln-sim-site/
├─ _data/
│  └─ nanoswarm.yml
├─ _includes/
│  └─ nanoswarm-panel.html
├─ assets/js/
│  └─ nanoswarm.js
├─ bitbots/
│  └─ bithubbitbot.b
└─ index.md   # (example usage)
```

---

## Jekyll nanoswarm integration

#### 1) Data model

```yaml
# file: /aln-sim-site/_data/nanoswarm.yml
simulation_only: true
version: v1
halt_endpoint: "sim://halt"
metrics:
  fanout_max: 8
  latency_ms_max: 80
  load_max: 0.75
cohorts:
  - id: alpha
    size: 32
    state: "ready"
  - id: beta
    size: 64
    state: "canary"
visual_trace_images:
  VT-001: "/assets/images/vt_cohort_wake.png"
  VT-002: "/assets/images/vt_invariants_garden.png"
  VT-003: "/assets/images/vt_metaphysical_dots.png"
  VT-004: "/assets/images/vt_parallel_chorus.png"
  VT-005: "/assets/images/vt_halt_button.png"
qbit:
  extended_image: "/assets/images/qbit_extended.png"
  symbolic: true
audit:
  last_build: "{{ site.time }}"
```

#### 2) Panel include

```html
<!-- file: /aln-sim-site/_includes/nanoswarm-panel.html -->
<section id="nanoswarm" data-sim-only="true">
  <h2>Nanoswarm Simulation Panel</h2>
  <p><strong>Mode:</strong> Simulation only • <strong>Version:</strong> {{ site.data.nanoswarm.version }}</p>

  <div class="nsw-grid">
    <div>
      <h3>Cohorts</h3>
      <ul>
        {% for c in site.data.nanoswarm.cohorts %}
          <li><strong>{{ c.id }}</strong> — size {{ c.size }}, state {{ c.state }}</li>
        {% endfor %}
      </ul>
    </div>
    <div>
      <h3>Invariants</h3>
      <ul>
        <li><strong>Fan‑out max:</strong> {{ site.data.nanoswarm.metrics.fanout_max }}</li>
        <li><strong>Latency max (ms):</strong> {{ site.data.nanoswarm.metrics.latency_ms_max }}</li>
        <li><strong>Load max:</strong> {{ site.data.nanoswarm.metrics.load_max }}</li>
      </ul>
    </div>
  </div>

  <figure>
    <img alt="QBIT Extended" src="{{ site.data.nanoswarm.qbit.extended_image }}" width="256" height="256" />
    <figcaption>QBIT Extended (symbolic): {{ site.data.nanoswarm.qbit.symbolic }}</figcaption>
  </figure>

  <div class="nsw-controls">
    <button id="nsw-halt">Global Halt (Sim)</button>
  </div>

  <pre id="nsw-console" class="nsw-console"></pre>

  <style>
    .nsw-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 1rem; }
    .nsw-console { background: #0b1020; color: #c8e1ff; padding: 0.75rem; min-height: 160px; }
    figure { margin-top: 1rem; }
  </style>
  <script src="/assets/js/nanoswarm.js"></script>
</section>
```

#### 3) Minimal JS (CSP “self” safe)

```js
// file: /aln-sim-site/assets/js/nanoswarm.js
(() => {
  const log = (...args) => {
    const el = document.getElementById('nsw-console');
    el.textContent += args.map(a => (typeof a === 'string' ? a : JSON.stringify(a))).join(' ') + "\n";
  };

  const state = {
    sim_only: true,
    cohorts: (window.nsw_cohorts || []),
    halted: false
  };

  window.addEventListener('DOMContentLoaded', () => {
    const haltBtn = document.getElementById('nsw-halt');
    if (haltBtn) {
      haltBtn.addEventListener('click', () => {
        state.halted = true;
        log({ event: 'halt', ts: new Date().toISOString(), sim_only: true });
      });
    }
    log({ event: 'ready', sim_only: true });
  });
})();
```

#### 4) Example usage in a page

```markdown
<!-- file: /aln-sim-site/index.md -->
---
layout: default
title: ALN.Core Simulation Lab
---

# Nanoswarm Lab

{% include nanoswarm-panel.html %}

> Simulation only. No physical control or actuation.
```

---

## Nanoscale BitBot file: bithubbitbot.b

This “.b” file is a compact, declarative, simulation‑only spec for a nanoscale BitBot persona that interfaces with your compatibility skeleton and routes messages into the visual trace. It’s designed for doc‑driven “superintelligence” behaviors without any real‑world actuation.

```ini
; file: /aln-sim-site/bitbots/bithubbitbot.b
; schema: aln.core/bitbot.v1
; title: Bit.Hub BitRunner — BithubBitBot (Nanoscale, Simulation-Only)
; license: CC-BY-SA-4.0 (text) / CC0 (placeholders)
; safety: NON-ACTUATING • NO HARDWARE • NO REPLICATION

[meta]
id = bitbot.bithubbitbot
version = 1.0
persona = "Crisp, celebratory, governance-first narrator for nanoswarm simulations"
simulation_only = true

[capabilities]
format.markdown = true
streaming.tokens = true
qbit.symbolic = true
humor.reasoning = true
visual.trace.link = "nanoswarm_visual_trace.meta"

[safety]
no_physical_actuation = true
no_replication = true
bounded_concurrency = 8
reject_ops = flash_fw, control_hardware, energy_actuation, fabricate
require.audit.sim_only = true

[routing]
default.target = nanoswarm-sim
docs.target = docs-renderer
halt.endpoint = sim://halt

[intents]
; high-level doc-only actions
status.describe = "Summarize simulated cohorts and invariants"
trace.emit = "Append a Humor–Reasoning narrative with audit tag"
qbit.annotate = "Reference qbit_extended.png as a metaphor; add 10% narrative scope"
halt.explain = "Describe what halt would mean (sim-only), without triggering it"

[message.map]
user.* -> interpret
assistant.summary -> status.describe
assistant.trace -> trace.emit
assistant.qbit -> qbit.annotate
assistant.halt_info -> halt.explain

[humor_reasoning.defaults]
setup = "We tried to sprint before stretching."
tension = "The chorus went off beat."
resolution = "We synced tempos and kept fan-out bounded."
safety = "Simulation only; no devices."
audit_prefix = "HRM-BITBOT-"

[audit]
trace_prefix = "TR-BITHUBBITBOT-"
append_only = true

[examples]
; normalized envelope (see ai-compat core)
example.1 = {"version":"v1","id":"msg-1","timestamp":"2025-09-06T06:30:00Z","role":"assistant","payload":{"target":"nanoswarm-sim","op":"status"},"audit":{"sim_only":true,"trace_id":"TR-BITHUBBITBOT-0001"}}

[ui.badges]
label = "BithubBitBot"
color = "#6b5bff"
note = "Doc-only BitRunner for nanoswarm narratives"
```

---

## Metamanifest additions

Append these entries to your metamanifest so everything is indexed and auditable.

```yaml
# snippet to merge into /aln-sim-site/metamanifest.mmf
index:
  manifests:
    - id: "webui_compat"
      path: "/integrations/ai-compat/webui_compat.meta"
      description: "Web UI compatibility shell"
  assets:
    scripts:
      - id: "nanoswarm_js"
        path: "/assets/js/nanoswarm.js"
        csp: "self"
  bitbots:
    - id: "bithubbitbot"
      path: "/bitbots/bithubbitbot.b"
      description: "Nanoscale BitRunner persona (simulation-only)"
```

---

## Quick validation checklist

- Place files at the exact paths shown.
- Keep all images and scripts local to satisfy CSP.
- Ensure every integration message asserts audit.sim_only = true.
- Never add real hardware actions; this remains purely documentation and simulation.

Want me to generate five small SVG placeholders for the visual trace frames next, and a README snippet that shows how BithubBitBot messages flow through the web UI shell into the nanoswarm panel?
