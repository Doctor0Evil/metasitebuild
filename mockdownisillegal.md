Hybrid corrector workflow and a .dont.comply.git sandbox
You’ll get a reusable-and-standalone corrector plus a quarantined playground that never blocks mainline work. The “.dont.comply.git” space is a visible, auditable sandbox with auto-repair, logs, and merge guards.

Reusable and standalone corrector
Place this at .github/workflows/bitbot-corrector-v2.yml. It can be called by other workflows (workflow_call) or run on demand (workflow_dispatch). It also auto-runs on pushes to the dont-comply branch namespace.

yaml
name: BitBot Corrector v2

on:
  workflow_call:
    inputs:
      repair_mode:
        type: boolean
        required: false
        default: true
      target_ref:
        type: string
        required: false
        default: main
  workflow_dispatch:
    inputs:
      repair_mode:
        description: Enable auto-fixes
        required: false
        type: boolean
        default: true
      target_ref:
        description: Target branch/ref to repair
        required: false
        type: string
        default: main
  push:
    branches:
      - dont-comply
      - dont-comply/**
    paths-ignore:
      - '**.md'

permissions:
  contents: write
  pull-requests: write

concurrency:
  group: corrector-${{ github.ref }}
  cancel-in-progress: false

jobs:
  corrector:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout
        uses: actions/checkout@v4

      - name: Sanity log
        run: |
          echo "[BitBot] Starting corrector v2"
          echo "repair_mode=${{ inputs.repair_mode }}"
          echo "target_ref=${{ inputs.target_ref }}"

      - name: Run repairs
        if: ${{ inputs.repair_mode == true }}
        run: |
          # your repair scripts go here (lint/format/fix)
          echo "Running auto-repairs..."
          # git add/commit only if diff exists
          if ! git diff --quiet; then
            git config user.name "bitbot"
            git config user.email "bitbot@users.noreply.github.com"
            git commit -am "bitbot: auto-repairs"
          fi

      - name: Open/Update PR to target_ref
        env:
          GH_TOKEN: ${{ secrets.GITHUB_TOKEN }}
        run: |
          CURRENT_BRANCH="${GITHUB_HEAD_REF:-${GITHUB_REF#refs/heads/}}"
          if [ "$CURRENT_BRANCH" != "${{ inputs.target_ref }}" ]; then
            gh pr list --head "$CURRENT_BRANCH" --base "${{ inputs.target_ref }}" --json number -q '.[0].number' | \
              { read N || true; \
                if [ -z "$N" ]; then
                  gh pr create --fill --base "${{ inputs.target_ref }}" --head "$CURRENT_BRANCH" \
                    --title "BitBot: repairs from $CURRENT_BRANCH" \
                    --label "repaired-by-bitbot" \
                    --body "Automated repairs applied by BitBot Corrector v2."
                else
                  gh pr edit "$N" --add-label "repaired-by-bitbot"
                  gh pr comment "$N" --body "BitBot Corrector v2 updated repairs."
                fi; }
          fi
Calling it from a meta workflow
Use a job-level uses: to call the reusable workflow. Note the secrets: inherit so the called workflow sees the same token scope.

yaml
# .github/workflows/meta-corrector.yml
name: Meta Corrector

on:
  workflow_dispatch:
    inputs:
      auto_fix:
        type: boolean
        default: true
      target_ref:
        type: string
        default: main

jobs:
  run-corrector:
    uses: ./.github/workflows/bitbot-corrector-v2.yml
    secrets: inherit
    with:
      repair_mode: ${{ inputs.auto_fix }}
      target_ref: ${{ inputs.target_ref }}
Tip: Do not call the reusable workflow from steps. It must be at jobs.<job_id>.uses.

Compliance wall guard for the sandbox
This guard enforces that anything in dont-comply must be repaired or explicitly reviewed before merge.

yaml
# .github/workflows/compliance-wall.yml
name: Compliance Wall

on:
  pull_request:
    branches: [ main, trunk, stable, release/* ]

permissions:
  contents: read
  pull-requests: write

jobs:
  guard:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Detect sandbox origin or paths
        id: detect
        run: |
          from_branch="${{ github.head_ref }}"
          echo "from_branch=$from_branch"
          touched_sandbox=$(git diff --name-only origin/${{ github.base_ref }}...HEAD | grep -E '^\.dont\.comply\.git/|^dont-comply/' || true)
          echo "touched_sandbox<<EOF" >> $GITHUB_OUTPUT
          echo "$touched_sandbox" >> $GITHUB_OUTPUT
          echo "EOF" >> $GITHUB_OUTPUT
          if [ -n "$touched_sandbox" ] || echo "$from_branch" | grep -q '^dont-comply'; then
            echo "sandbox=true" >> $GITHUB_OUTPUT
          else
            echo "sandbox=false" >> $GITHUB_OUTPUT
          fi

      - name: Enforce labels and status
        if: steps.detect.outputs.sandbox == 'true'
        env:
          GH_TOKEN: ${{ secrets.GITHUB_TOKEN }}
        run: |
          pr="${{ github.event.pull_request.number }}"
          gh pr edit "$pr" --add-label "sandbox"
          # Require BitBot repair label before allowing merge
          if ! gh pr view "$pr" --json labels -q '.labels[].name' | grep -q '^repaired-by-bitbot$'; then
            echo "Sandbox changes must be repaired/acknowledged."
            echo "::error title=Compliance Wall::Add 'repaired-by-bitbot' or pass review by CODEOWNERS."
            exit 1
          fi
Repo scaffolding for .dont.comply.git
Purpose: A quarantined, auditable playground. Safe-by-default, celebrated when repaired.

.gitattributes (root)

gitattributes
# Normalize line endings
* text=auto eol=lf

# Treat ALN/rego sensibly
*.aln diff=yaml
*.rego diff=rego

# Sandbox zone is quarantined content
.dont.comply.git/** -diff -merge linguist-vendored export-ignore
CODEOWNERS (root)

text
# Require governance review for sandbox
.dont.comply.git/**  @org/compliance-team
dont-comply/**       @org/compliance-team
.gitignore (root)

gitignore
# Ignore sandbox build artifacts
.dont.comply.git/**/build/
.dont.comply.git/**/dist/
Branching convention

dont-comply/ branches: ephemeral, auto-repaired, never protected.

main/trunk: protected; PRs touching sandbox require repaired-by-bitbot or compliance review.

Fast checklist
Define hybrid corrector: on: workflow_call + workflow_dispatch + push to dont-comply.

Call it correctly: jobs.<id>.uses with secrets: inherit.

Add guard: compliance-wall.yml blocks unsafe merges from sandbox.

Scaffold sandbox: .dont.comply.git/ folder, CODEOWNERS, .gitattributes, .gitignore.

Protect branches: require status checks and PR reviews on main/trunk.

If you want, I can tailor the corrector’s “repairs” step to your actual linters, manifest fixers, and rescue/rename cycles so the audit trail reads like BitBots throwing confetti while they heal the tree.
