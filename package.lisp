;;;; package.lisp

(defpackage #:lawk
  (:use #:cl
	#:cl-ppcre
	#:split-sequence
	#:alexandria)
  (:export #:lawk
	   #:lawk-file
	   #:*read-record*
	   #:*split-record*))

