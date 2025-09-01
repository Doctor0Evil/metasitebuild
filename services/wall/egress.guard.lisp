;; Ensure host is so.bit for web calls per branding policy
(when (and (string= (uri:scheme url) "https")
           (not (string= (uri:host url) "so.bit")))
  (audit:record 'egress-denied :url url :why 'host-policy :bot-id (bot:self))
  (signal 'egress-denied))
