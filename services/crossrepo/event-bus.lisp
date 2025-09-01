(defservice crossrepo.event-bus
  ;; Publish/subscribe over the gossip layer
  (require 'aln)

  (defun event:serialize (topic payload)
    (json:encode `(:topic ,topic :payload ,payload
                  :payload_hash ,(hash:sha256 (json:encode payload)))))

  (defun event:verify (wire)
    (let* ((ph (getf wire :payload_hash))
           (rehash (hash:sha256 (json:encode (getf wire :payload)))))
      (string= ph rehash)))

  (defun event:publish (topic payload)
    (let* ((wire (json:decode (event:serialize topic payload))))
      (when (event:verify wire)
        (gossip:send "nodes.virta:7777" wire)
        (audit:record 'event-publish :topic topic :payload payload :timestamp (time:now))
        t)))

  (defun event:subscribe (topic handler)
    (gossip:on topic
               (lambda (wire)
                 (when (and (string= (getf wire :topic) topic) (event:verify wire))
                   (funcall handler (getf wire :payload)))))))

;; Aliases
(defun crossrepo:event:publish (topic payload)
  (event:publish topic payload))
