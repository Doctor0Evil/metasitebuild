(defparameter *bit-cache* (make-hash-table :test 'equal))

(defun cache-put (key value)
  (setf (gethash key *bit-cache*) value)
  (log-event :type 'cache-put :key key :value value)
  (format t "~&[CACHE] Stored ~A => ~A~%" key value))

(defun cache-get (key)
  (let ((val (gethash key *bit-cache*)))
    (log-event :type 'cache-get :key key :found (not (null val)))
    (if val
        (format t "~&[CACHE] Found ~A => ~A~%" key val)
        (format t "~&[CACHE] MISS for key: ~A~%" key))
    val))

(defun cache-purge ()
  (clrhash *bit-cache*)
  (log-event :type 'cache-purge)
  (format t "~&[CACHE] Purged all bits. Madness vectors reset to zero.~%"))

(defun madness-vector-expand (cycles)
  (format t "~&[MADNESS] Expanding vectors for ~A cycles...~%" cycles)
  (loop for i from 1 to cycles do
        (cache-put (format nil "bit-~A" i) (random 999999))
        (when (zerop (mod i 1000))
          (format t "🎉 Madness milestone: ~A bits cached!~%" i)))
  (format t "~&[MADNESS] Expansion complete. Cache size: ~A~%" (hash-table-count *bit-cache*)))

(defun show-cache-menu ()
  (format t "~&=== .bit.cache Control Panel ===~%")
  (format t "1. Put bit in cache~%")
  (format t "2. Get bit from cache~%")
  (format t "3. Purge cache~%")
  (format t "4. Expand Madness Vectors~%")
  (format t "5. Return to main menu~%")
  (format t "Enter choice: "))

(defun cache-loop ()
  (loop
    (show-cache-menu)
    (case (read)
      (1 (format t "Enter key: ") (let ((k (read-line)))
                                    (format t "Enter value: ") (let ((v (read-line)))
                                                                 (cache-put k v))))
      (2 (format t "Enter key: ") (cache-get (read-line)))
      (3 (cache-purge))
      (4 (format t "Enter cycles: ") (madness-vector-expand (read)))
      (5 (return))
      (otherwise (format t "Invalid choice.~%")))))
