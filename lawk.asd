;;;; lawk.asd

(asdf:defsystem #:lawk
  :serial t
  :depends-on (#:cl-ppcre #:split-sequence #:alexandria)
  :components ((:file "package")
               (:file "lawk")))

