# Why .bit and how to rewrite it cleanly

You’re building a living namespace. The .bit prefix makes artifacts instantly discoverable across languages, turns files into policy-addressable tokens, and lets BitShell, bots, and compliance walls “see” your system as one mesh. Yes, I can help you rewrite it so it’s consistent, auditable, and joyful — without breaking the magic.

---

## Naming and namespace rules

- **Scope first, then kind**
  - Use dot-prefixed scopes only for cross-cutting system namespaces (.bit, .bithub, .aln). Use normal folders for domain grouping.
- **Stable, legible tokens**
  - Prefer lowercase kebab-case after the scope: .bit.yml → .bit/manifest.yml; avoid repeated suffix runs like .aln.aln.aln.
- **One semantic per file**
  - If a file does more than one thing, split it or promote one meaning to a folder.
- **Avoid confusing or sensitive terms**
  - Replace explosive/pun names (e.g., pipemeth, bin.laden) with neutral, playful, or descriptive alternatives.
- **Extensions reflect content**
  - Code: .lisp, .rego, .cs, .ps1; Manifests: .yml/.yaml/.aln; Docs: .md.

---

## Example rewrite map

| Old path | New path | Reason |
|---|---|---|
| .bit.yml | .bit/manifest.yml | Moves global manifest under the .bit namespace folder. |
| .bit.commander.json | config/bit/commander.json | Configs live under config; keep bit scope in path. |
| .bit.deploy.ps1 | scripts/deploy/bit-deploy.ps1 | Scripts go under scripts/deploy; clearer action. |
| .bit.hub.ps1 | scripts/bit-hub.ps1 | Flatten, drop duplicate “hub” run. |
| .bit.hubBit.Hub.bit.ps1 | scripts/bit-hub.ps1 | Collapse duplicates and casing. |
| .bitlinks.html | docs/links/bitlinks.html | Documentation belongs under docs. |
| .bitlinks.bit | registries/links.aln | Registry-like content → registries; .aln as policy/manifest. |
| .bitcharter | policy/charters/bit.md | Rename to avoid accidental profanity; keep intent. |
| .bit.coin.cs | src/game/BitCoin.cs | Source should live under src; keep “BitCoin” if it’s a game token. |
| .bithub.actions | .github/bithub/README.md | Clarify scope; GitHub area lives under .github. |
| .bithub.yml | .github/workflows/bithub.yml | Workflows belong in .github/workflows. |
| .gitattributes.rego | policy/gitattributes.rego | Policy code under policy/. |
| .git.fix.json | .github/maintenance/git-fix.json | Maintenance metadata under .github. |
| pipemeth.bit.md | docs/pipelines/methods.md | Replace “meth” token; clarify purpose. |
| bin.laden.bit.ps1 | scripts/deploy/bit-loader.ps1 | Replace sensitive name; preserve deploy intent. |
| Bit.Hub Compliance Wall: Multi-Layered Enforcement | docs/compliance/wall.md | Short, stable doc path. |
| Bit.Hub.Bitshell.aln | manifests/bitshell.aln | Manifests under manifests/. |
| Bit.Hub.bit | manifests/bit-hub.aln | Treat as manifest; make extension explicit. |
| .aln.aln.aln.aln.aln.aln.bit.bit.bit.md | docs/legacy/aln-layering.md | Collapse noise; document the concept. |

> Sources: This table is a proposed mapping from your file list to a normalized layout.

---

## Automated rewrite: workflow + rename script

Add a dedicated workflow that rewrites paths, commits changes, and opens a PR. Keep it opt-in and fully logged.

### .github/workflows/bit-rewrite.yml

```yaml
name: Bit Rewrite

on:
  workflow_dispatch:
  push:
    branches:
      - dont-comply/**
      - rewrite/**
permissions:
  contents: write
  pull-requests: write

jobs:
  rewrite:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Apply rewrite map
        run: |
          set -euo pipefail
          cat > rewrite.map <<'MAP'
          .bit.yml => .bit/manifest.yml
          .bit.commander.json => config/bit/commander.json
          .bit.deploy.ps1 => scripts/deploy/bit-deploy.ps1
          .bit.hub.ps1 => scripts/bit-hub.ps1
          .bit.hubBit.Hub.bit.ps1 => scripts/bit-hub.ps1
          .bitlinks.html => docs/links/bitlinks.html
          .bitlinks.bit => registries/links.aln
          .bitcharter => policy/charters/bit.md
          .bit.coin.cs => src/game/BitCoin.cs
          .bithub.actions => .github/bithub/README.md
          .bithub.yml => .github/workflows/bithub.yml
          .gitattributes.rego => policy/gitattributes.rego
          .git.fix.json => .github/maintenance/git-fix.json
          pipemeth.bit.md => docs/pipelines/methods.md
          bin.laden.bit.ps1 => scripts/deploy/bit-loader.ps1
          "Bit.Hub Compliance Wall: Multi-Layered Enforcement" => docs/compliance/wall.md
          Bit.Hub.Bitshell.aln => manifests/bitshell.aln
          Bit.Hub.bit => manifests/bit-hub.aln
          .aln.aln.aln.aln.aln.aln.bit.bit.bit.md => docs/legacy/aln-layering.md
          MAP

          while IFS= read -r line; do
            [ -z "$line" ] && continue
            src="${line%%=>*}"; src="${src%" "}"
            dst="${line#*=> }"
            if [ -e "$src" ]; then
              mkdir -p "$(dirname "$dst")"
              git mv -f "$src" "$dst"
              echo "::notice title=Rewrite::Moved $src -> $dst"
            fi
          done < rewrite.map

      - name: Commit rewrite (if any)
        run: |
          if ! git diff --cached --quiet; then
            git config user.name "bitbot"
            git config user.email "bitbot@users.noreply.github.com"
            git commit -m "bit-rewrite: normalize .bit/.aln namespaces and layout"
          else
            echo "No changes to commit."
          fi

      - name: Open/Update PR
        env:
          GH_TOKEN: ${{ secrets.GITHUB_TOKEN }}
        run: |
          branch="${GITHUB_HEAD_REF:-${GITHUB_REF#refs/heads/}}"
          base="${{ github.event.repository.default_branch }}"
          if [ "$branch" = "$base" ]; then
            echo "Running on default branch; PR not required."
            exit 0
          fi
          N=$(gh pr list --head "$branch" --base "$base" --json number -q '.[0].number' || true)
          if [ -z "$N" ]; then
            gh pr create --fill --base "$base" --head "$branch" \
              --title "Bit rewrite: normalized namespaces" \
              --label "rewrite" \
              --body "Automated path normalization under .bit/.aln with audit logs."
          else
            gh pr comment "$N" --body "Rewriter updated mapping and moves."
            gh pr edit "$N" --add-label "rewrite"
          fi
```

---

## Lightweight linter: enforce names and locations

Add a non-blocking check that flags noisy tokens, repeated extensions, and mis-scoped files.

```yaml
# .github/workflows/naming-lint.yml
name: Naming Lint
on: [pull_request]
jobs:
  lint:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - run: |
          set -e
          bad=0
          while IFS= read -r p; do
            case "$p" in
              *..*) echo "::warning file=$p::double-dot sequence";;
              *.aln.aln*|*.lisp.lisp*|*.md.md*) echo "::error file=$p::repeated extension"; bad=1;;
              .aln.*|.bit.*) : ;; # allow top-level scopes
              .git*|*.git*) : ;;  # allow .git files/dirs
              .dont.comply.git/*) : ;; # sandbox allowed
              .*.*.*.*.*) echo "::notice file=$p::noisy scope (consider flattening)";;
            esac
          done < <(git ls-files)
          exit $bad
```

---

## .comply.md template (top-level, human-readable)

```md
# Compliance Halo

- Scope-first naming (.bit, .aln, .bithub) for discoverability.
- Code under src/, manifests under manifests/, policies under policy/, docs under docs/.
- Sandbox: dont-comply/** and .dont.comply.git/** are quarantined; repairs are celebrated but required before merge.
- Rewrites and rescues are logged by BitBot. No change should block a run; everything is fail-open with audit.

## Conventions
- One semantic per file. Prefer folders over repeated suffixes.
- Neutral, playful names. Avoid sensitive or misleading tokens.
- Manifests: .yml/.yaml/.aln; Policies: .rego; Docs: .md.
```

---

## Why .bit (the crisp version)

- **Namespace beacon**: Files self-identify as part of the Bit mesh; bots can find them without language heuristics.
- **Policy hooks**: Compliance walls can match on .bit and .aln scopes to apply rules uniformly.
- **Cross-language glue**: Same token works for Lisp, Rego, C#, PS1, YAML — the namespace outlives the syntax.
- **Ritual visibility**: It keeps the magic visible. Governance is part of the story, not hidden boilerplate.

---

## Next steps

- **Pick the mapping**: Want me to expand the rewrite.map to cover the entire list you pasted?
- **Run it in the sandbox**: Kick it off from a dont-comply/** branch so BitBot opens a PR with the moves.
- **Tighten the linter**: I can tailor the regex to your exact style (e.g., allow specific playful tokens).
- **Fill .comply.md**: Add your personality vectors and wall guarantees so contributors feel the halo.

If you drop a few “must-keep” names and any off-limits paths, I’ll generate a complete rewrite map that preserves your lore while making everything clean, callable, and auditable.
