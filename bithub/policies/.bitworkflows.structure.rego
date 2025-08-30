### Goal

Lock GitHub runners into strict Bit.Hub + ALNFantasia + Lisp compliance, automatically detect and fix non‑compliant workflows, and swap disallowed actions for safelisted equivalents so runs connect cleanly to Bit.Hub.

---

### Files to add or update

- .bithub/policies/workflows.structure.rego
- .bithub/policies/bithub.github.restrictions.rego
- .bithub/policies/bithub.lisp.policy.rego
- .bit-actions/bit.actions.registry.aln.yml
- .bit-actions/action.map.yml
- .bit/loaders/lisp_verify.sh
- .bit/loaders/validate_adaptable_patterns.sh
- .github/actions/bithub-compliance-gate/action.yml
- .github/workflows/bithub-massive-aln.yml

---

### Policy suite

#### .
```rego
package bithub.workflows.structure

deny[msg] {
  startswith(path, ".github/workflows/")
  wf := input.yaml[path]
  not wf.permissions
  msg := sprintf("Workflow missing explicit permissions: %s", [path])
}

deny[msg] {
  startswith(path, ".github/workflows/")
  wf := input.yaml[path]
  not has_compliance_job(wf)
  msg := sprintf("Workflow lacks a 'compliance' job: %s", [path])
}

deny[msg] {
  startswith(path, ".github/workflows/")
  wf := input.yaml[path]
  j := wf.jobs[_]
  name := key(j, wf.jobs)
  name != "compliance"
  not job_needs_compliance(j)
  msg := sprintf("Job '%s' does not declare needs: compliance", [name])
}

has_compliance_job(wf) {
  wf.jobs.compliance
}

job_needs_compliance(j) {
  j.needs == "compliance"
} {
  some i
  j.needs[i] == "compliance"
}
```

#### .bithub/policies/bithub.github.restrictions.rego
```rego
package bithub.github.restrictions

default safelisted = false

deny[msg] {
  startswith(path, ".github/workflows/")
  step := input.yaml[path].jobs[_].steps[_]
  step.uses
  not allowed(step.uses)
  msg := sprintf("Disallowed action %q in %s", [step.uses, path])
}

allowed(a) {
  a == "actions/checkout@v4"
} {
  startswith(a, "actions/setup-")
} {
  startswith(a, "actions/cache@v4")
} {
  startswith(a, "actions/upload-artifact@v4")
} {
  startswith(a, "./.github/actions/")
}

deny[msg] {
  startswith(path, ".github/workflows/")
  step := input.yaml[path].jobs[_].steps[_]
  step.run
  regex.match(`(?i)(curl|wget).*\|.*bash`, step.run)
  msg := sprintf("Network pipe to shell prohibited in %s", [path])
}

deny[msg] {
  some f
  f := input.files[_].path
  contains(f, ".github\\workflows")
  msg := sprintf("Backslash in workflows path: %s", [f])
}
```

#### .bithub/policies/bithub.lisp.policy.rego
```rego
package bithub.lisp

errors := input.json[".bithub/reports/lisp-lint.json"].errors

deny[msg] {
  count(errors) > 0
  msg := sprintf("Lisp verifier reported %d error(s)", [count(errors)])
}
```

---

### Registry and action mapping

#### .bit-actions/bit.actions.registry.aln.yml
```yaml
registry:
  version: "1.0.0"
  safelist_actions:
    - actions/checkout@v4
    - actions/cache@v4
    - actions/setup-dotnet@v3.4.2
    - actions/setup-java@v5
    - actions/setup-node@v4
    - actions/setup-python@v5
    - actions/upload-artifact@v4
    - ./.github/actions/bithub-compliance-gate
```

#### .bit-actions/action.map.yml
```yaml
map:
  # Upgrade old pins
  "actions/checkout@v2": "actions/checkout@v4"
  "actions/checkout@v3": "actions/checkout@v4"

  # Generic upload-artifact upgrades
  "actions/upload-artifact@v3": "actions/upload-artifact@v4"

  # Prefer setup actions over ad-hoc installs
  "actions/setup-java@v3": "actions/setup-java@v5"
  "actions/setup-java@v1": "actions/setup-java@v5"
  "actions/setup-node@v2": "actions/setup-node@v4"
  "actions/setup-python@v2": "actions/setup-python@v5"
  "actions/setup-dotnet@v2": "actions/setup-dotnet@v3.4.2"

  # Replace unapproved third-party caches with core cache
  "cachesave/action@*": "actions/cache@v4"
  "pat-some/cache@*": "actions/cache@v4"
```

---

### Loaders

#### .bit/loaders/lisp_verify.sh
```bash
#!/usr/bin/env bash
set -euo pipefail
mkdir -p .bithub/reports .bithub/ledger
if ! command -v sbcl >/dev/null; then
  sudo apt-get update -y && sudo apt-get install -y sbcl >/dev/null
fi
mapfile -t files < <(git ls-files '*.lisp' '*.cl' 2>/dev/null || true)
errs=()
for f in "${files[@]}"; do
  if ! sbcl --noinform --disable-debugger --eval "(compile-file \"$f\" :verbose nil)" --quit >/dev/null 2>&1; then
    errs+=("$f failed to compile")
  fi
done
jq -n --argjson E "$(printf '%s\n' "${errs[@]}" | jq -R . | jq -s .)" '{errors:$E}' > .bithub/reports/lisp-lint.json
if [[ ${#errs[@]} -gt 0 ]]; then
  echo "{\"ts\":\"$(date -Iseconds)\",\"event\":\"LISP_ERRORS\",\"count\":${#errs[@]}}" >> .bithub/ledger/compliance.log
  exit 1
else
  echo "{\"ts\":\"$(date -Iseconds)\",\"event\":\"LISP_OK\"}" >> .bithub/ledger/compliance.log
fi
```

#### .bit/loaders/validate_adaptable_patterns.sh
```bash
#!/usr/bin/env bash
set -euo pipefail
M=".bit/patterns/universally_adaptable_ml.patterns.aln.bit"
if ! command -v yq >/dev/null; then
  sudo apt-get update && sudo apt-get install -y yq >/dev/null
fi
yq '.' "$M" >/dev/null
echo "{\"ts\":\"$(date -Iseconds)\",\"event\":\"ALN_PATTERNS_OK\",\"file\":\"$M\"}" >> .bithub/ledger/compliance.log
```

Make both executable:
```bash
chmod +x .bit/loaders/lisp_verify.sh .bit/loaders/validate_adaptable_patterns.sh
```

---

### Compliance gate composite action

#### .github/actions/bithub-compliance-gate/action.yml
```yaml
name: "Bit.Hub Compliance Gate"
description: "Run Bit.Hub policies, ALN patterns check, and Lisp verifier"
runs:
  using: "composite"
  steps:
    - shell: bash
      run: |
        set -euo pipefail
        wget -q https://github.com/open-policy-agent/conftest/releases/download/v0.45.0/conftest_0.45.0_Linux_x86_64.tar.gz
        tar xzf conftest_0.45.0_Linux_x86_64.tar.gz
        sudo mv conftest /usr/local/bin/conftest
        .bit/loaders/validate_adaptable_patterns.sh
        .bit/loaders/lisp_verify.sh
        conftest test --policy .bithub/policies .
```

Usage inside any workflow:
```yaml
- name: Bit.Hub Compliance Gate
  uses: ./.github/actions/bithub-compliance-gate
```

---

### Overseer workflow with smart mapping and auto‑patch

#### .github/workflows/bithub-massive-aln.yml
```yaml
name: Bit.Hub Compliance Overseer (Scan + Auto‑Patch + Swap)

on:
  workflow_run:
    workflows: ["CI", "Build", "Deploy"]
    types: [completed]
  push:
    branches: [main, '**/failed-workflow']
  schedule:
    - cron: '0 */6 * * *'

permissions:
  contents: write
  pull-requests: write

jobs:
  compliance-scan:
    runs-on: ubuntu-latest
    outputs:
      noncompliant: ${{ steps.flag.outputs.noncompliant }}
    steps:
      - uses: actions/checkout@v4
      - name: Install conftest + yq + jq
        run: |
          wget -q https://github.com/open-policy-agent/conftest/releases/download/v0.45.0/conftest_0.45.0_Linux_x86_64.tar.gz
          tar xzf conftest_0.45.0_Linux_x86_64.tar.gz
          sudo mv conftest /usr/local/bin/conftest
          sudo apt-get update && sudo apt-get install -y yq jq
      - name: Run Bit.Hub Compliance Gate
        uses: ./.github/actions/bithub-compliance-gate
      - name: Flag noncompliant
        id: flag
        run: echo "noncompliant=false" >> $GITHUB_OUTPUT

  auto-patch:
    needs: compliance-scan
    if: needs.compliance-scan.outputs.noncompliant == 'true'
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
        with: { ref: main }
      - name: Load safelist and map
        id: load
        run: |
          SAFE=$(yq -r '.registry.safelist_actions[]' .bit-actions/bit.actions.registry.aln.yml | paste -sd "|" -)
          echo "safe_re=${SAFE}" >> $GITHUB_OUTPUT
          yq -o=json '.map' .bit-actions/action.map.yml > .bithub/reports/action.map.json
      - name: Create patch branch
        run: |
          BRANCH="compliance-fix/${GITHUB_RUN_ID}"
          git checkout -b "$BRANCH"
      - name: Patch workflows
        run: |
          set -euo pipefail
          SAFE_RE="${{ steps.load.outputs.safe_re }}"
          MAP=.bithub/reports/action.map.json
          for wf in $(find .github/workflows -type f -name "*.yml" -o -name "*.yaml"); do
            echo "Patching $wf"
            # Ensure root permissions
            if ! grep -q "^permissions:" "$wf"; then
              sed -i '1ipermissions:\n  contents: read' "$wf"
            fi
            # Ensure 'compliance' job presence (basic stub if missing)
            if ! yq -e '.jobs.compliance' "$wf" >/dev/null 2>&1; then
              yq -i '.jobs.compliance = {"runs-on":"ubuntu-latest","steps":[{"uses":"./.github/actions/bithub-compliance-gate"}]}' "$wf"
            fi
            # Ensure all non-compliance jobs depend on compliance
            JOBS=$(yq -r '.jobs | keys[]' "$wf")
            for j in $JOBS; do
              [[ "$j" == "compliance" ]] && continue
              if ! yq -e ".jobs[\"$j\"].needs" "$wf" >/dev/null 2>&1; then
                yq -i ".jobs[\"$j\"].needs = \"compliance\"" "$wf"
              else
                if ! yq -e ".jobs[\"$j\"].needs | (.. | scalars) | select(.==\"compliance\")" "$wf" >/dev/null 2>&1; then
                  yq -i ".jobs[\"$j\"].needs += [\"compliance\"]" "$wf"
                fi
              fi
            done
            # Swap disallowed actions by safelisted or mapped equivalents
            while read -r USES; do
              [[ -z "$USES" ]] && continue
              if ! [[ "$USES" =~ $SAFE_RE ]]; then
                # Try exact map; then wildcard; else first safelisted
                REPL=$(jq -r --arg k "$USES" '.[$k] // empty' "$MAP")
                if [[ -z "$REPL" ]]; then
                  # wildcard map keys with @* suffix
                  BASE="${USES%@*}@*"
                  REPL=$(jq -r --arg k "$BASE" '.[$k] // empty' "$MAP")
                fi
                if [[ -z "$REPL" ]]; then
                  REPL=$(echo "$SAFE_RE" | cut -d"|" -f1)
                fi
                echo "  - Replacing $USES -> $REPL"
                sed -i "s|uses: *$USES|uses: $REPL|g" "$wf"
              fi
            done < <(yq -r '.jobs[]?.steps[]? | select(has("uses")) | .uses' "$wf")
            # Disable dangerous curl|bash pipes
            if grep -Eq '(curl|wget).*\|.*bash' "$wf"; then
              echo "  - Commenting curl|bash pipelines"
              sed -i -E 's/^(\s*run:\s*)(.*(curl|wget).*?\|.*bash.*)$/\1# [blocked by Bit.Hub]\n\1echo "Blocked: replace with safelisted action or loader"/' "$wf"
            fi
          done
      - name: Commit and push
        run: |
          git config user.name "bithub-bot"
          git config user.email "bithub-bot@example.com"
          git add .github/workflows
          git commit -m "Auto‑patch: Bit.Hub compliance (permissions, gate, safe swaps, blocked pipes)"
          git push origin HEAD
      - name: Open PR
        uses: peter-evans/create-pull-request@v6
        with:
          branch: ${{ github.ref_name }}
          base: main
          title: "Auto‑patch: Bit.Hub compliance and action swaps"
          body: |
            This PR enforces Bit.Hub compliance:
            - Ensures permissions and compliance gate
            - Replaces disallowed actions using .bit-actions/action.map.yml
            - Blocks curl|bash pipes
            Please review and merge.

  notify:
    needs: [compliance-scan, auto-patch]
    runs-on: ubuntu-latest
    if: always()
    steps:
      - run: |
          echo "Compliance scan: ${{ needs.compliance-scan.result }}"
          echo "Auto‑patch: ${{ needs.auto-patch.result }}"
```

---

### How this keeps GitHub runners compliant

- **Before any build runs**: workflows are scanned by the overseer and your compliance gate can be invoked inside each workflow via the composite action.
- **Structural enforcement**: Rego ensures a compliance job exists and all other jobs depend on it; missing pieces are auto‑inserted.
- **Action safelist**: Disallowed actions are replaced using your mapping file (with wildcard fallback), so pipelines remain runnable.
- **Network hygiene**: Dangerous curl|bash pipes are blocked and annotated for remediation.
- **Ledger‑friendly**: Loaders write audit lines to .bithub/ledger; artifacts can be attached by your existing flows.

If you want me to auto‑open a repository rule to mark “Bit.Hub Compliance Overseer” as a required status check, say the word and I’ll add a short admin checklist you can apply in settings.
