(defservice databank.ingest
  (require 'json)
  (require 'hash)
  (require 'ed25519)

  ;; Verify headers and signature, write append-only
  (defun verify-request (body hex-hash hex-pub hex-sig)
    (let* ((calc (hash:sha256 body)))
      (unless (string= calc hex-hash)
        (error "Payload hash mismatch"))
      (unless (ed25519:verify hex-pub hex-hash hex-sig)
        (error "Signature invalid"))
      t))

  (defun append-violation (repo body)
    (let ((path (format nil "databank/~A/violations.alnlog" repo)))
      (fs:mkdirs (fs:dirname path))
      (with-open-file (s path :direction :output :if-exists :append :if-does-not-exist :create)
        (write-line body s))))

  (defun handle-ingest (req)
    (let* ((repo (or (http:query req "repo") "unknown"))
           (hex-hash (http:header req "X-Payload-Hash"))
           (hex-pub  (http:header req "X-Public-Key"))
           (hex-sig  (http:header req "X-Signature"))
           (body (http:body-bytes req)))
      (verify-request body hex-hash hex-pub hex-sig)
      (append-violation repo (bytes:to-string body))
      (http:respond 202 "accepted")))

  (http:route :post "/ingest/violation" #'handle-ingest)

  (defun handle-status (req)
    (let* ((repo (http:query req "repo"))
           (path (format nil "databank/~A/violations.alnlog" repo))
           (violations (and (fs:exists? path) (> (fs:size path) 0))))
      (http:json 200 `(:violations ,violations :last_hash ,(ledger:last-hash "registries/command-ledger.alnlog")))))

  (http:route :get "/ingest/status" #'handle-status)

  (http:start :host "0.0.0.0" :port 8443))
