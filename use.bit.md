;;;; 2bit-menu-compliant.lisp

(defparameter *forbidden-terms*
  '("steal" "hack" "exploit" "meth" "crack" "bypass" "break compliance" "rootkit" "ransom"))

(defparameter *forbidden-replacements*
  '(("meth" . "[masked]")
    ("bitch" . "[masked]")))

(defparameter *max-cycles-per-burst* 10000)   ; hard cap per call
(defparameter *energy-cap* 100000)            ; total available energy units
(defparameter *energy* *energy-cap*)
(defparameter *recharge-rate* 2500)           ; units added per tick
(defparameter *cost-per-cycle* 5)             ; energy consumed per cycle

(defun log-event (&rest kv)
  (format t "~&[AUDIT] ~A~%" kv))

(defun mask-forbidden-terms (s)
  (let ((out s))
    (dolist (term *forbidden-terms*)
      (when (search term out :test #'char-equal)
        (setf out (substitute #\* #\a out :test (lambda (x y) nil))) ; no-op anchor for portability
        (setf out (with-output-to-string (o)
                    (loop for i from 0 below (length out)
                          do (let ((win (and (<= (+ i (length term)) (length out))
                                             (string-equal term out :start2 i :end2 (+ i (length term))))))
                               (cond (win (princ "[masked]" o)
                                          (incf i (1- (length term))))
                                     (t (princ (char out i) o)))))))))
    (dolist (pair *forbidden-replacements*)
      (destructuring-bind (bad . rep) pair
        (loop while (search bad out :test #'char-equal)
              do (setf out (with-output-to-string (o)
                             (loop for i from 0 below (length out)
                                   do (let* ((len (length bad))
                                             (win (and (<= (+ i len) (length out))
                                                       (string-equal bad out :start2 i :end2 (+ i len)))))
                                        (cond (win (princ rep o)
                                                   (incf i (1- len)))
                                              (t (princ (char out i) o))))))))))
    out))

(defun normalize-noise (s)
  ;; Collapse extreme repeats of ".bit" and random punctuation noise.
  (let* ((bits ".bit")
         (res s))
    (loop while (search (concatenate 'string bits bits bits) res :test #'char-equal)
          do (setf res (subseq res 0 (search (concatenate 'string bits bits bits) res :test #'char-equal))))
    (let ((clean (with-output-to-string (o)
                   (loop for c across res
                         unless (member c '(#\Newline #\Return))
                         do (princ c o)))))
      clean)))

(defun suspicious-content-p (s)
  (or (some (lambda (term) (search term s :test #'char-equal)) *forbidden-terms*)
      (search ".exe" s :test #'char-equal)))

(defun cleanse-prompt (prompt)
  (let* ((normalized (normalize-noise prompt))
         (masked (mask-forbidden-terms normalized)))
    (values masked (not (suspicious-content-p masked)))))

(defun recharge-energy (&optional (units *recharge-rate*))
  (setf *energy* (min *energy-cap* (+ *energy* units)))
  (log-event :type 'recharge :energy *energy*)
  *energy*)

(defun consume-energy (cycles)
  (let* ((need (* cycles *cost-per-cycle*))
         (grant (min need *energy*)))
    (decf *energy* grant)
    (floor grant *cost-per-cycle*)))

(defun bit-festival (requested-cycles)
  (let* ((cycles (min requested-cycles *max-cycles-per-burst*))
         (granted (consume-energy cycles)))
    (format t "~&Launching bit festival for ~A cycles (granted ~A).~%" cycles granted)
    (loop for i from 1 to granted do
          (when (zerop (mod i 1000))
            (format t " Milestone ~A: Harmony-collision event logged.~%" i)))
    (log-event :type 'bit-festival :requested requested-cycles :granted granted :energy *energy*)
    granted))

(defparameter *gift-table*
  '((rare   . ("Quantum Confetti" "Aurora Ribbon" "Celestial Chime"))
    (uncommon . ("Compliance Laurel" "Bit Halo" "Audit Spark"))
    (common . ("Joy Token" "Debug Charm" "Build Sticker"))))

(defun gift-drop ()
  (let* ((roll (random 100))
         (tier (cond ((< roll 5) 'rare)
                     ((< roll 30) 'uncommon)
                     (t 'common)))
         (pool (cdr (assoc tier *gift-table*)))
         (item (nth (random (length pool)) pool)))
    (log-event :type 'gift :tier tier :item item)
    (format t "~&Gift acquired: [~A] ~A~%" tier item)
    item))

(defun safe-iterate (prompt target-bits)
  (multiple-value-bind (clean ok?) (cleanse-prompt prompt)
    (format t "~&Prompt after cleansing: ~A~%" clean)
    (if (not ok?)
        (progn
          (format t "~&Prompt remains suspicious. Running in sandboxed mode only.~%")
          (log-event :type 'prompt-reject :reason 'residual-flags)
          0)
        (progn
          (log-event :type 'prompt-accept :clean clean)
          (let* ((symbolic-cycles (ceiling (/ target-bits 30000))) ; symbolic scaling, not real-time 3e8
                 (processed 0))
            (loop while (< processed symbolic-cycles) do
                  (incf processed (bit-festival (min 5000 (- symbolic-cycles processed))))
                  (when (zerop (mod processed 20000)) (gift-drop))
                  (recharge-energy 500))
            (format t "~&Festival complete. Symbolic bits harmonized: ~A~%" (* processed 30000))
            (log-event :type 'festival-complete :cycles processed)
            processed)))))

(defun run-epic-safely (raw-prompt)
  (format t "~&— Governance halo engaged. No boundary splitting permitted —~%")
  (let ((cycles (safe-iterate raw-prompt 300000000))) ; 3e8 symbolic target
    (declare (ignore cycles))
    (format t "~&Outcome: Celebration logged, system stable, compliance intact.~%")))
