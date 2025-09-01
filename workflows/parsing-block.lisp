(defblock parsing.block
  ;; Validate ALN + policy before execution
  (let* ((policy (aln:load "manifests/content.policy.aln"))
         (script (aln:load "scripts/BitShellALN.ps1.aln")))
    (assert policy "Failed to load content policy.")
    (assert script "Failed to load BitShellALN script.")

    (audit:record 'aln-parse
                  :timestamp (time:now)
                  :bot-id (bot:self)
                  :files '("manifests/content.policy.aln"
                           "scripts/BitShellALN.ps1.aln"))

    ;; Optional transformation hook
    (when (manifest:enabled? 'aln-transform)
      (setf script (aln:transform script))
      (log:info "ALN transform applied.")
      (store:artifact "aln.transformed" script)))

  t)
