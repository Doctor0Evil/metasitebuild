# tools/guards/no-raw-profanity.sh
set -euo pipefail
# Reconstruct deny tokens from masked seeds at runtime and scan the staged diff.
tmp=$(mktemp)
cat > "$tmp" <<'LISP'
(load "src/AI/profanity/seeds.masked.lisp")
(load "src/AI/profanity/generator.lisp")
(in-package :bit.banter)
(dolist (b *seed-bases*)
  (format t "~a~%" (unmask b)))
LISP
TOKENS=$(sbcl --noinform --non-interactive --script "$tmp" | tr 'A-Z' 'a-z' | sort -u)
DIFF=$(git diff --cached -U0 | tr 'A-Z' 'a-z')
while read -r t; do
  [ -z "$t" ] && continue
  if echo "$DIFF" | grep -Fq " $t"; then
    echo "Blocked commit: raw explicit token detected. Use masked seeds + runtime generator."
    exit 1
  fi
done <<< "$TOKENS"
