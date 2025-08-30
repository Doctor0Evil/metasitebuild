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
