(defworkflow bots-deploy-example
  ;; Recursive deploy orchestrator with PR fallback and bot attribution
  (let ((bot-id (bot:self))
        (repo (git:current-repo))
        (max-attempts 3))
    (log:info (format nil "Deploy initiated by ~A on ~A" bot-id repo))

    (loop for attempt from 1 to max-attempts
          do
            (log:info (format nil "Attempt ~A/~A" attempt max-attempts))

            ;; Ensure policy + script are present
            (assert (fs:exists? "manifests/content.policy.aln")
                    "Missing manifests/content.policy.aln")
            (assert (fs:exists? "scripts/BitShellALN.ps1.aln")
                    "Missing scripts/BitShellALN.ps1.aln")

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
