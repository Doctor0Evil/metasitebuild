(deflib fan.asia.decompress
  ;; Export standard and mesh-compliant decompression utilities
  (export bitdecomp-zlib)
  (export bitdecomp-gzip)
  (export bitdecomp-tar)
  (export bitdecomp-ns-artifact)
  (export bitdecomp-auditlog)
  ;; Railguard for compliance safety
  (export bitdecomp-railguard)
)

;; --- ZLIB (universal) ---
(defun bitdecomp-zlib (input output &key (max-size 128MB) (policy "strict") (audit t))
  (if (> (file-size input) max-size)
      (error "Aborted: decompression size limit exceeded for compliance"))
  (shell "python3 -c 'import zlib,sys;sys.stdout.buffer.write(zlib.decompress(sys.stdin.buffer.read()))'" :in input :out output)
  (when audit (fan.asia.decompress::bitdecomp-auditlog input output "zlib" policy)))

;; --- GZIP ---
(defun bitdecomp-gzip (input output &key (policy "strict"))
  (shell "gzip -dc < ~A > ~A" input output)
  (fan.asia.decompress::bitdecomp-auditlog input output "gzip" policy))

;; --- TAR / TAR.GZ ---
(defun bitdecomp-tar (archive destdir &key (gzip nil) (policy "hybrid"))
  (let ((cmd (if gzip "tar -xzf ~A -C ~A" "tar -xf ~A -C ~A")))
    (shell cmd archive destdir)
    (fan.asia.decompress::bitdecomp-auditlog archive destdir (if gzip "tar.gz" "tar") policy)))

;; --- Namespaced Artifact Decompression (Bit.Hub Mesh) ---
(defun bitdecomp-ns-artifact (uri destdir &key (policy "hybrid"))
  "Decompresses any ns/s3/ipfs mesh artifact using universal Bit.Hub mesh tools."
  (let ((tmp "/tmp/bitdecomp-temp"))
    (shell "bitmesh fetch ~A --out ~A" uri tmp)
    (bitdecomp-tar tmp destdir :gzip t :policy policy)
    (shell "rm -f ~A" tmp)))

;; --- Audit-logged railguard ---
(defun bitdecomp-auditlog (input output method policy)
  (let ((entry (json-encode `(,@(list :ts (iso8601-now) :event "DECOMPRESSION"
                                      :input input :output output :method method :policy policy)))))
    (append-to-log ".bithub/ledger/bitdecomp.log" entry)))

;; --- Real-time compliance railguard ---
(defun bitdecomp-railguard (action &key (opa-policy ".bithub/policies/decompression.rego"))
  (let ((res (shell "opa eval --data ~A 'input.action == \"~A\"' --format raw" opa-policy action)))
    (unless (string= res "true")
      (error "Decompression action is blocked by Bit.Hub compliance policy"))))
