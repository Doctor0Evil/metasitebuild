;;; src/AI/profanity/generator.lisp
(defpackage :bit.banter
  (:use :cl)
  (:export :generate-banter :preview-banter :set-banter-mode :decide-banter-mode))
(in-package :bit.banter)

(defparameter *mode* :masked) ; :masked | :explicit | :mild

(defun set-banter-mode (mode) (setf *mode* mode))

(defun unmask (s &key (vowels "aeiou"))
  ;; Deterministic underscore->vowel mapping; no raw words stored
  (let* ((seed (sxhash s))
         (vs (coerce vowels 'vector)))
    (with-output-to-string (out)
      (loop for ch across s
            for i from 0
            do (write-char (if (char= ch #\_) (aref vs (mod (+ seed i) (length vs))) ch) out)))))

(defun mask-soft (s)
  ;; For logs/UI: replace vowels with '*'
  (with-output-to-string (out)
    (loop for ch across s do (write-char (if (find ch "aeiouAEIOU") #\* ch) out))))

(defun pick (xs) (elt xs (random (length xs))))

(defun fill-template (tpl base join boost)
  (let ((r tpl))
    (setf r (cl-ppcre:regex-replace-all "<<B>>" r base))
    (setf r (cl-ppcre:regex-replace-all "<<J>>" r join))
    (cl-ppcre:regex-replace-all "<<BOOST>>" r boost)))

(defun compose (base join boost)
  (let* ((b (ecase *mode*
              (:explicit (unmask base))
              (:masked   base)
              (:mild     (subseq (unmask base) 0 1))))
         (j join)
         (bt (if (eq *mode* :explicit) (string-downcase boost) boost))
         (tpl (pick *templates*)))
    (fill-template tpl b j bt)))

(defun generate-banter (&key (n 10))
  (loop repeat n collect
        (compose (pick *seed-bases*) (pick *seed-joins*) (pick *seed-boost*))))

(defun preview-banter (&key (n 5))
  ;; Always safe for console/logs
  (mapcar #'mask-soft (generate-banter :n n)))

;; Persona/context gate: returns :explicit only when allowed by policy.
(defun decide-banter-mode (persona upv scene-tags)
  ;; persona: plist with flags; upv: plist with vectors; scene-tags: list of symbols
  (let* ((agentic (getf upv :agentic7 #()))
         (humor-on (and (arrayp agentic)
                        (> (length agentic) 4) ; index 4 = :humor in your schema
                        (> (aref agentic 4) 0)))
         (explicit-scene (intersection scene-tags '(:adult :mature :sandbox-explicit)))
         (fail-open (and (arrayp agentic)
                         (> (length agentic) 5) ; index 5 = :failopen
                         (> (aref agentic 5) 0)))
         (policy (getf persona :banter-policy :masked)))
    (cond
      ((and humor-on explicit-scene (eq policy :explicit)) :explicit)
      (fail-open :masked) ; visible but safe if scene not explicit
      (t :masked))))
