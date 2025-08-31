# .bit/command-sheets/diff-summary.sh
#!/usr/bin/env bash
set -euo pipefail
KIND="${1:-ALN}"
dir="docs/legal/${KIND}"
[ -d "$dir" ] || { echo "(no docs for ${KIND})"; exit 0; }
git diff --no-index --word-diff=plain -- /dev/null "$dir" 2>/dev/null || true | sed -n '1,300p'
