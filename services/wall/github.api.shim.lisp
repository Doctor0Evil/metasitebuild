(defservice github.api.shim
  ;; Translate common GitHub API intents to internal registries; only passthrough via egress.guard when explicitly allowlisted.
  (defun gh:open-pr (repo branch target labels)
    (internal:pr:open repo branch target labels) ; internal fallback
    (when (manifest:enabled? 'passthrough-github)
      (handler-case
          (wall:http :post (format nil "https://api.github.com/repos/~A/pulls" repo)
                     '(("Accept" . "application/vnd.github+json"))
                     (json:encode `(("head" . ,branch) ("base" . ,target) ("labels" . ,labels))))
        (egress-denied () (log:warn "PR passthrough denied by wall.")))))
  (defun gh:status (repo sha state desc)
    (internal:status:write repo sha state desc)
    (when (manifest:enabled? 'passthrough-github)
      (ignore-errors
        (wall:http :post (format nil "https://api.github.com/repos/~A/statuses/~A" repo sha)
                   '(("Accept" . "application/vnd.github+json"))
                   (json:encode `(("state" . ,state) ("description" . ,desc))))))))
