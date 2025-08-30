(defpackage :nanobyte-deploy
  (:use :cl :aln))
(in-package :nanobyte-deploy)

(defun sys.deploy (package)
  (declare (special *console-output*))
  (let ((chk (verify-package package)))
    (if chk
        (progn
          (register-to-console package)
          (integrate-bitbot package)
          (emit-signal :deploy-complete package)
          (setf *console-output* "Parser launched and awaiting jobs. No errors. All integrations active."))
        (setf *console-output* "Deployment failed: package verification error.")))
  *console-output*)
