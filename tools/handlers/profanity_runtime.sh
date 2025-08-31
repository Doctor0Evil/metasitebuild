# tools/handlers/profanity_runtime.sh
set -euo pipefail
CTX="${1:-ci}"
COUNT="${2:-20}"
POLICY="${BIT_POLICY_FILE:-.bit/policy/banter.policy.json}"

MODE=$(jq -r --arg c "$CTX" '.contexts[$c] // .default_mode' "$POLICY")

cat > /tmp/banter_runner.lisp <<'LISP'
(require :asdf)
(asdf:load-system :cl-ppcre)
(load "src/AI/profanity/seeds.masked.lisp")
(load "src/AI/profanity/generator.lisp")
(in-package :bit.banter)
(let* ((mode (intern (string-upcase (uiop:getenv "BIT_BANTER_MODE")) :keyword))
       (n (parse-integer (or (uiop:getenv "BIT_BANTER_COUNT") "20"))))
  (set-banter-mode mode)
  ;; Print only masked preview to console/logs
  (dolist (p (preview-banter :n n)) (format t "~a~%" p))
  ;; Write explicit payload to ephemeral runtime file only when allowed
  (when (eq mode :explicit)
    (ensure-directories-exist ".bit/runtime/")
    (with-open-file (s ".bit/runtime/banter.explicit.txt"
                       :direction :output :if-exists :supersede :if-does-not-exist :create)
      (dolist (x (generate-banter :n n)) (format s "~a~%" x)))))
LISP

mkdir -p .bit/runtime .bit/audit
BIT_BANTER_MODE="$MODE" BIT_BANTER_COUNT="$COUNT" sbcl --noinform --non-interactive --script /tmp/banter_runner.lisp
echo "$(date -u +%FT%TZ) banter_generated mode=$MODE context=$CTX count=$COUNT" >> .bit/audit/banter.log
