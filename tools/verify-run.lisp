#!/usr/bin/env sbcl --script
(require 'aln)
(require 'rego)
(require 'json)

(defun file-sha256 (path)
  (hash:sha256 (fs:read-bytes path)))

(defun load-policy ()
  (let ((path "manifests/master-enforcement.aln"))
    (unless (verify:ed25519 path ".keys/ed25519.public")
      (error "Policy signature invalid"))
    (aln:load path)))

(defun evaluate-run (policy context)
  (let* ((rego-module ".bithub/policies/compliance-wall.rego")
         (params (aln:get policy 'policies 'parameters))
         (bindings (aln:get policy 'bindings))
         (input (append context params bindings))
         (result (rego:eval-file rego-module :input input)))
    result))

(defun build-certificate (context result)
  (let ((cert `(:ts ,(time:now)
                :bot ,(bot:self)
                :repo ,(git:current-repo)
                :context ,context
                :decision ,(if (getf result :deny) "deny" "allow")
                :rationale ,(or (getf result :deny) "all invariants satisfied")
                :manifest_hash ,(file-sha256 "manifests/master-enforcement.aln")
                :ledger_hash ,(ledger:last-hash "registries/command-ledger.alnlog"))))
    (json:encode cert)))

(defun sign-certificate (cert-json)
  (let ((sig (ed25519:sign ".keys/ed25519.private" (hash:sha256 cert-json))))
    (list :certificate cert-json :signature sig :pubkey (fs:read ".keys/ed25519.public"))))

;; Main
(let* ((context `(:domain ,(net:current-domain)
                  :role ,(runner:role)
                  :encryption ,(env:get "ENCRYPTION")
                  :key_path ,(env:get "KEY_PATH")
                  :region ,(env:get "REGION")))
       (policy (load-policy))
       (result (evaluate-run policy context))
       (cert-json (build-certificate context result))
       (signed (sign-certificate cert-json)))
  (format t "~A~%" (json:encode signed)))
