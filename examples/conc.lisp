;A simple concordence translated from my AWK version
;Notice how "NR" from awk is not builtin but can be easily emulated
(declaim (optimize (speed 3)))
(defun word-split (s)
  (remove "" (split-sequence:split-sequence-if (lambda (c) (not (alpha-char-p c))) s) :test #'equal))

(defun maph (function ht)
  (let ((result nil)) (maphash (lambda (k v) (push (funcall function k v) result)) ht) result))

(defun conc (fname)
  (lawk:lawk-file (fname f
                     (nr 0)
                     (words (make-hash-table :test #'equalp))
                     (lawk:*split-record* #'word-split))
    (t (incf nr))
    (t (let ((uniques nil))
         (mapc (lambda (x) (pushnew x uniques :test #'equalp)) (cdr f))
         (mapc (lambda (x) (push nr (gethash x words nil))) uniques)))
    (:end 
     (mapc #'write-line (sort (maph (lambda (k v) (format nil "~a ~{~a~^,~}" k (nreverse v))) words) #'string<)))))
