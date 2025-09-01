(defworkflow bots-deploy-example
  ;; Required modules and services
  (require 'services/wall/egress.guard.lisp)
  (require 'services/wall/github.api.shim.lisp)
  (require 'workflows/verify-ledger.lisp)
  (require 'registries/assets/indexer.lisp)
  (require 'aln)
  (require 'rego)

  ;; ------------------------------------------------------------
  ;; Utilities
  ;; ------------------------------------------------------------

  (defun plist-merge (&rest plists)
    "Left-to-right shallow merge of property lists."
    (let ((out '()))
      (dolist (pl plists)
        (loop for (k v) on pl by #'cddr do (setf (getf out k) v)))
      out))

  (defun file-sha256 (path)
    (hash:sha256 (fs:read-bytes path)))

  ;; ------------------------------------------------------------
  ;; Manifest loading + verification + audit/ledger append
  ;; ------------------------------------------------------------

  (defun load-master-manifest ()
    (let* ((path "manifests/master-enforcement.aln")
           (manifest (aln:load path))
           (hash (file-sha256 path)))
      ;; Verify signature of the manifest before trusting it
      (unless (verify:ed25519 path ".keys/ed25519.public")
        (error "Manifest signature invalid."))

      ;; Append a manifest-hash entry to the command ledger (tamper-evident)
      (ledger:append "registries/command-ledger.alnlog"
                     `(:ts ,(time:now)
                       :bot ,(bot:self)
                       :repo ,(git:current-repo)
                       :payload ,hash
                       :signature ""               ; optional: use detached signature if you keep one
                       :pubkey ,(fs:read ".keys/ed25519.public")
                       :prev_hash ,(ledger:last-hash "registries/command-ledger.alnlog")
                       :entry_hash ""))

      ;; Audit record for manifest load
      (audit:record 'manifest-load
                    :path path
                    :hash hash
                    :bot-id (bot:self)
                    :timestamp (time:now))
      manifest))

  ;; ------------------------------------------------------------
  ;; OPA/Rego policy enforcement
  ;; ------------------------------------------------------------

  (defun enforce-master-policy (manifest context)
    (let* ((rego-module ".bithub/policies/compliance-wall.rego")
           (params (aln:get manifest 'policies 'parameters))
           (bindings (aln:get manifest 'bindings))
           (input (plist-merge context params bindings))
           (result (rego:eval-file rego-module :input input)))
      ;; Record policy decision (allow/deny and any messages) for auditability
      (audit:record 'policy-decision
                    :rego-module rego-module
                    :input input
                    :result result
                    :timestamp (time:now))
      ;; Deny if policy signals a denial
      (when (getf result :deny)
        (error (format nil "Compliance wall denial: ~A" (getf result :deny))))
      result))

  ;; ------------------------------------------------------------
  ;; Main recursive deploy flow with PR fallback
  ;; ------------------------------------------------------------

  (let* ((bot-id (bot:self))
         (repo (git:current-repo))
         (max-attempts 3)
         (success nil))

    (log:info (format nil "Deploy orchestrator started by ~A on ~A" bot-id repo))

    ;; Bounded self-healing loop
    (loop for attempt from 1 to max-attempts
          do
            (log:info (format nil "Deploy attempt ~A/~A" attempt max-attempts))
            (handler-case
                (progn
                  ;; Fresh manifest load and enforcement each attempt
                  (let* ((manifest (load-master-manifest))
                         (context `(:domain ,(net:current-domain)
                                    :role ,(runner:role)
                                    :encryption ,(env:get "ENCRYPTION")
                                    :key_path ,(env:get "KEY_PATH"))))
                    (enforce-master-policy manifest context))

                  ;; Pre-deploy checks and indexing
                  (parsing.block)
                  (assets.indexer)
                  (verify-ledger)

                  ;; Execute ALN-wrapped pipeline
                  (let ((ok (aln:exec "scripts/BitShellALN.ps1.aln"
                                      :env `((BOT_ID . ,bot-id)
                                             (REPO . ,repo)))))
                    (if ok
                        (progn
                          (log:info "Deploy succeeded.")
                          (setf success t)
                          (return t))
                        (log:warn "Pipeline reported failure."))))
              (error (e)
                (log:warn (format nil "Attempt ~A failed: ~A" attempt e))))
            (unless success
              (log:warn "Attempt failed; preparing next try...")))

    ;; Fallback PR + escalation if not successful
    (unless success
      (log:error "All attempts failed. Initiating fallback PR and escalation.")
      (let* ((branch (git:create-branch (format nil "bot-fallback/~A" (time:stamp))))
             (patch (aln:generate-patch "scripts/BitShellALN.ps1.aln" :context "deploy-fix"))
             (manifest-hash (file-sha256 "manifests/master-enforcement.aln"))
             (ledger-hash (ledger:last-hash "registries/command-ledger.alnlog"))
             (meta (format nil "Manifest: ~A~%Ledger: ~A" manifest-hash ledger-hash)))
        (git:commit branch patch
                    :message (format nil "[bot] fallback patch by ~A~%~A" bot-id meta))
        ;; Internal first; shim will attempt passthrough only if allowed
        (internal:pr:open repo branch "main" '("bot-fallback"))
        (gh:open-pr repo branch "main" '("bot-fallback"))
        (log:info "Fallback PR opened.")
        (notify:human 'devops-team :context 'deploy-failure)))

    success))
