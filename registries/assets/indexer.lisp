(defservice assets.indexer
  ;; Walk assets/, generate signed index, append to ledger
  (let* ((root "assets")
         (index "registries/assets/index.aln")
         (schema (aln:load "manifests/asset.schema.aln"))
         (policy (aln:load "manifests/compliance.wall.aln"))
         (entries '()))
    (fs:walk root
             (lambda (path)
               (when (fs:file? path)
                 (let* ((hash (hash:sha256 (fs:read-bytes path)))
                        (class (content:classify policy path))
                        (meta `(:path ,path :hash ,hash :class ,class :size ,(fs:size path))))
                   (push meta entries)))))

    (assert (schema:valid? entries schema) "Asset index schema violation.")
    (aln:store index entries)

    ;; Sign and ledger
    (let* ((payload (hash:sha256 (json:encode entries)))
           (sig (ed25519:sign ".keys/ed25519.private" payload)))
      (ledger:append "registries/command-ledger.alnlog"
                     `(:ts ,(time:now)
                       :bot ,(bot:self)
                       :repo ,(git:current-repo)
                       :payload ,payload
                       :signature ,sig
                       :pubkey ,(fs:read ".keys/ed25519.public")
                       :prev_hash ,(ledger:last-hash "registries/command-ledger.alnlog")
                       :entry_hash "")))
    (log:info "Asset index updated and recorded.")
    t))

(defun content:classify (policy path)
  (cond
    ((glob:match path "assets/**") "dev_asset")
    ((glob:match path "docs/**") "literature")
    (t "misc")))
