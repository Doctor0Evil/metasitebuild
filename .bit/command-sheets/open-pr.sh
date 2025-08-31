# .bit/command-sheets/open-pr.sh
#!/usr/bin/env bash
set -euo pipefail
BRANCH="aln/legal-evolution-$(date -u +%Y%m%d%H%M%S)"
git config user.name "BitHub-Bot"
git config user.email "bot@bithub.local"
git checkout -b "$BRANCH" || git checkout "$BRANCH"
git add -A
if git diff --cached --quiet; then
  echo "No changes to propose."
  exit 0
fi
git commit -m "ALN Legal Evolution: update drafted docs"
git push -u origin "$BRANCH" || { echo "Push failed; leaving changes committed locally."; exit 0; }

if command -v gh >/dev/null 2>&1; then
  gh pr create --title "ALN Legal Evolution update" \
               --body "Automated legal drafting update. See job summary for diffs." \
               --label "legal-update" --base "main" || true
  # Optional: request reviews
  # gh pr edit --add-reviewer "<team-or-user>" || true
else
  echo "gh not available; PR not opened (fail-open)."
fi
