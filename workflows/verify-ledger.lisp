(defworkflow verify-ledger
  (let* ((ledger "registries/command-ledger.alnlog")
         (pubkey ".keys/ed25519.public")
         (entries (ledger:tail ledger 200)))
    (assert entries "Ledger empty or missing.")

    (loop for e in entries
          for idx from 0
          do
            (let* ((prev (when (> idx 0) (getf (nth (- idx 1) entries) :entry_hash)))
                   (rehash (hash:sha256 (json:encode (plist:subset e '(:ts :bot :repo :payload :signature :pubkey :prev_hash)))))
                   (sig-ok (ed25519:verify pubkey (getf e :payload) (getf e :signature))))
              (assert (string= (getf e :entry_hash) rehash) "Entry hash mismatch.")
              (assert (or (null prev) (string= (getf e :prev_hash) prev)) "Chain broken.")
              (assert sig-ok "Signature invalid.")))
    (log:info "Ledger verified: chain intact, signatures valid.")
    t))
