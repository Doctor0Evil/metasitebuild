;;; src/AI/profanity/persona_gate.lisp
(defun banter-mode-for-runner (persona upv scene-tags policy)
  ;; persona: plist (e.g., :id, :banter-policy)
  ;; upv: materialized UPV plist with :agentic7 etc.
  ;; scene-tags: e.g., '(:adult :pvp)
  ;; policy: parsed JSON from .bit/policy/banter.policy.json
  (let* ((ctx-mode (getf policy (or (car (intersection scene-tags '(:production-explicit :staging :ci :local-dev)))
                                    :default_mode)))
         (decided (bit.banter:decide-banter-mode persona upv scene-tags)))
    ;; Final mode is the stricter of context vs persona gate
    (if (and (eql decided :explicit) (string= ctx-mode "explicit"))
        :explicit
        :masked)))
