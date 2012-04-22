;;;; lawk.lisp

(in-package #:lawk)
(declaim (optimize (speed 3)))

;;; "lawk" goes here. Hacks and glory await!

(defparameter +whitespace+ '(#\Space #\Tab #\Newline #\Page #\Return #\Linefeed))
(defparameter *read-record* (lambda (x) (read-line x nil)))
(defparameter *split-record* (lambda (x) (split-sequence-if (lambda (c) (member c +whitespace+)) x)))

(defun maph (function ht)
  (let ((result nil))
    (maphash (lambda (k v) (push (funcall function k v) result)))
    result))

(defun /x/ (regex)
  (let ((regex (create-scanner regex)))
  (lambda (fields &rest r)
    (declare (ignore r))
    (not (null (scan regex (nth 0 fields)))))))

(defmacro lawk ((stream metavar &rest bindings) &body b)
  (once-only (stream)
    (loop while b
	 for (rl . code) = (pop b)
	 for rule = (eval rl)
	 if (eq rule :begin) collect `(lambda () ,@code) into begins
	 else if (eq rule :end) collect `(lambda () ,@code) into ends
	 else if (eql rule t) collect `(cons (constantly t) (lambda (,metavar) (declare (ignorable ,metavar)) ,@code)) into others
	 else collect `(cons ,rule (lambda (,metavar) (declare (ignorable ,metavar)) ,@code)) into others
	 finally (return
		   `(let ,bindings (lawk::do-lawk ,stream ,(cons 'cl:list begins) ,(cons 'cl:list ends) ,(cons 'cl:list others)))))))

(defun do-lawk (stream begins ends others)
  (mapcar #'funcall begins)
  (loop for line = (funcall *read-record* stream)
       while line
       for fields = (cons line (funcall *split-record* line))
       do (mapcar (lambda (x) (when (funcall (car x) fields) (funcall (cdr x) fields))) others))
  (mapcar #'funcall ends)
  nil)
       
(defmacro lawk-file ((file metavar &rest bindings) &body b)
  (with-gensyms (stream)
    `(with-open-file (,stream ,file)
       (lawk (,stream ,metavar ,@bindings) ,@b))))

 (defmacro lawk-stream ((strm metavar &rest bindings) &body b)
  (with-gensyms (stream)
    `(with-open-stream (,stream ,strm)
       (lawk (,stream ,metavar ,@bindings) ,@b))))   

