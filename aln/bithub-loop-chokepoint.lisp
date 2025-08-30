(defpackage :bithub-loop-chokepoint
  (:use :cl :aln))

(in-package :bithub-loop-chokepoint)

(defvar *workflow-loop-database* (make-hash-table))
(defvar *bitbot-log* nil)

(defun record-workflow-state (run-id data)
  (setf (gethash run-id *workflow-loop-database*) data)
  (push data *bitbot-log*))

(defun bitbot-variable-validate (vars)
  (every (lambda (v) (and (stringp v) (not (search "-" v)) (string= (string-upcase v) v))) vars))

(defun audit-and-loop ()
  (dolist (run (all-active-workflows))
    (let ((data (get-workflow-data run)))
      (record-workflow-state (workflow-id run) data)
      (unless (bitbot-variable-validate (extract-variables data))
        (flag "parser-error" run))
      (when (unsafe-state-detected data)
        (call-bitbot data)))))

(defun call-bitbot (data)
  ;; External integration placeholder
  (format t "[BitBot] Safety triggered for workflow: ~A~%" data))
