(defworkflow master-crossrepo
  ;; -------------------------------
  ;; Required modules and services
  ;; -------------------------------
  (require 'services/wall/egress.guard.lisp)
  (require 'services/crossrepo/event-bus.lisp)
  (require 'workflows/verify-ledger.lisp)
  (require 'registries/assets/indexer.lisp)
  (require 'aln)
  (require 'rego)

  ;; -------------------------------
  ;; Utilities
  ;; -------------------------------
  (defun plist-merge (&rest plists)
    (let ((out '()))
      (dolist (pl plists)
        (loop for (k v) on pl by #'cddr do (setf (getf out k) v)))
      out))

  (defun file-sha256 (path)
    (hash:sha256 (fs:read-bytes path)))

  ;; -------------------------------
  ;; Manifest loading + verification + audit/ledger append
  ;; -------------------------------
  (defun load-master-manifest ()
    (let* ((path "manifests/master-enforcement.aln")
           (manifest (aln:load path))
           (hash (file-sha256 path)))
      (unless (verify:ed25519 path ".keys/ed25519.public")
        (error "Master enforcement manifest signature invalid."))
      (ledger:append "registries/command-ledger.alnlog"
                     `(:ts ,(time:now)
                       :bot ,(bot:self)
                       :repo ,(git:current-repo)
                       :payload ,hash
                       :signature ""
                       :pubkey ,(fs:read ".keys/ed25519.public")
                       :prev_hash ,(ledger:last-hash "registries/command-ledger.alnlog")
                       :entry_hash ""))
      (audit:record 'manifest-load
                    :path path
                    :hash hash
                    :bot-id (bot:self)
                    :timestamp (time:now))
      manifest))

  ;; -------------------------------
  ;; OPA/Rego policy enforcement
  ;; -------------------------------
  (defun enforce-master-policy (manifest context)
    (let* ((rego-module ".bithub/policies/compliance-wall.rego")
           (params (aln:get manifest 'policies 'parameters))
           (bindings (aln:get manifest 'bindings))
           (input (plist-merge context params bindings))
           (result (rego:eval-file rego-module :input input)))
      (audit:record 'policy-decision
                    :rego-module rego-module
                    :input input
                    :result result
                    :timestamp (time:now))
      (when (getf result :deny)
        (error (format nil "Compliance wall denial: ~A" (getf result :deny))))
      result))

  ;; -------------------------------
  ;; Stack runner (local and remote)
  ;; -------------------------------
  (defun run-stack-locally (repo stack-file)
    (log:info (format nil "Local stack run for ~A using ~A" repo stack-file))
    (parsing.block)
    (assets.indexer)
    (verify-ledger)
    (let* ((stack (aln:load stack-file))
           (steps (aln:get stack 'pipeline 'steps)))
      (dolist (s steps)
        (case (getf s :kind)
          (:build (aln:exec "scripts/BitShellALN.ps1.aln" :env `((BOT_ID . ,(bot:self)) (REPO . ,repo) (STACK . ,stack-file))))
          (:test  (aln:exec "scripts/BitShellALN.ps1.aln" :env `((BOT_ID . ,(bot:self)) (REPO . ,repo) (STACK . ,stack-file) (MODE . "test"))))
          (:scan  (workflow:run "workflows/verify-ledger.lisp"))
          (:publish (internal:publish repo stack))
          (t (log:warn (format nil "Unknown step kind: ~A" (getf s :kind)))))))
    t)

  (defun run-stack-remotely (repo node stack-file)
    (let ((job `(:repo ,repo :stack ,stack-file :requested_by ,(bot:self))))
      (crossrepo:event:publish "jobs" job)))

  ;; -------------------------------
  ;; Cross-repo orchestrator loop
  ;; -------------------------------
  (let* ((bot-id (bot:self))
         (manifest (load-master-manifest))
         (targets (aln:load "registries/crossrepo/targets.aln")))
    (log:info (format nil "Cross-repo orchestrator started by ~A" bot-id))
    (dolist (t (aln:get targets 'targets))
      (let* ((repo (getf t :repo))
             (node (getf t :node))
             (stack (getf t :stack))
             (context `(:domain ,(net:current-domain)
                        :role ,(runner:role)
                        :encryption ,(env:get "ENCRYPTION")
                        :key_path ,(env:get "KEY_PATH")
                        :target_repo ,repo
                        :target_node ,node)))
        (handler-case
            (progn
              ;; COMPLIANCE ENFORCEMENT: this call blocks all non-compliant actions from GitHub, Microsoft, Google unless explicitly allowed by your compliance wall policy!
              (enforce-master-policy manifest context)
              (if (and node (not (string= node "")))
                  (run-stack-remotely repo node stack)
                  (run-stack-locally repo stack))
              (log:info (format nil "Cross-repo task queued/executed for ~A" repo)))
          (error (e)
            (log:warn (format nil "Cross-repo task failed for ~A: ~A" repo e))
            ;; Fallback: open internal PR with provenance
            (let* ((branch (git:create-branch (format nil "bot-fallback/~A" (time:stamp))))
                   (patch  (aln:generate-patch stack :context "stack-fix"))
                   (manifest-hash (file-sha256 "manifests/master-enforcement.aln"))
                   (ledger-hash (ledger:last-hash "registries/command-ledger.alnlog"))
                   (meta (format nil "Manifest: ~A~%Ledger: ~A" manifest-hash ledger-hash)))
              (git:commit branch patch :message (format nil "[bot] crossrepo fallback ~A~%~A" repo meta))
              (internal:pr:open repo branch "main" '("bot-fallback"))
              (gh:open-pr repo branch "main" '("bot-fallback")))))))
    (log:info "Cross-repo orchestrator completed.")
    t))
