(defworkflow bots-deploy-example
  (let ((bot-id (bot:self))
        (repo (git:current-repo)))
    (require 'services/wall/egress.guard.lisp)
    (require 'services/wall/github.api.shim.lisp)
    (require 'workflows/verify-ledger.lisp)
    (require 'registries/assets/indexer.lisp)

    (parsing.block)
    (assets.indexer)     ;; index + ledger
    (verify-ledger)      ;; ensure continuity

    ;; Try deploy with local resources only
    (let ((ok (aln:exec "scripts/BitShellALN.ps1.aln"
                        :env `((BOT_ID . ,bot-id) (REPO . ,repo)))))
      (if ok
          (log:info "Local deploy succeeded.")
          (progn
            (log:warn "Local deploy failed; opening internal PR and attempting shim passthrough when allowed.")
            (internal:pr:open repo "bot-fallback" "main" '("bot-fallback"))
            (gh:open-pr repo "bot-fallback" "main" '("bot-fallback")))))
    t))
   ;; Parse and validate ALN before running
            (parsing.block)

            ;; Execute ALN-wrapped PowerShell pipeline
            (let ((ok (aln:exec "scripts/BitShellALN.ps1.aln"
                                :env `((BOT_ID . ,bot-id)
                                       (REPO . ,repo)))))
              (if ok
                  (progn
                    (log:info "Deploy succeeded.")
                    (return t))
                  (progn
                    (log:warn "Deploy failed. Preparing PR fallback.")
                    (let* ((branch (git:create-branch
                                    (format nil "bot-fallback/~A" (time:stamp))))
                           (patch (aln:generate-patch "scripts/BitShellALN.ps1.aln"
                                                      :context "deploy-fix")))
                      (git:commit branch patch
                                  :message (format nil "[bot] fallback patch by ~A" bot-id))
                      (git:open-pr branch :target "main" :label "bot-fallback")
                      (log:info "Fallback PR opened."))))))

    (unless (deploy:success?)
      (log:error "Exhausted attempts. Escalating.")
      (notify:human 'devops-team :context 'deploy-failure))))
