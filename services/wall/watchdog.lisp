(defservice wall.watchdog
  ;; Continuous verification of policy, pins, mirrors, and ledger continuity
  (defun check:policy-signature ()
    (verify:ed25519 "manifests/compliance.wall.aln" ".keys/ed25519.public"))
  (defun check:gha-pins ()
    (and (fs:exists? "configs/pin/gha-pins.aln")
         (pins:validate "configs/pin/gha-pins.aln")))
  (defun check:mirrors ()
    (mirror:validate '("tools/actions/cache" "tools/toolchains")))
  (defun check:ledger ()
    (workflow:run "workflows/verify-ledger.lisp"))
  (defun watchdog:run ()
    (loop
      (unless (and (check:policy-signature) (check:gha-pins) (check:mirrors) (check:ledger))
        (audit:record 'wall-violation :ts (time:now) :bot-id (bot:self))
        (repair:attempt))
      (sleep 30)))
  (watchdog:run))
