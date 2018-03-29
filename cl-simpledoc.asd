;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defsystem :cl-simpledoc
  :name ":cl-simpledoc"
  :description "Extracts Common Lisp documentation strings from symbols in a package and generates html output."
  :author "Shannon Spires <ssos@bearlanding.com>"
  :version "0.1"
  :maintainer "Shannon Spires <ssos@bearlanding.com>"
  :licence "BSD 3-clause"
  :depends-on (:closer-mop)
  :serial t
  :components ((:file "package")
               (:file "cl-simpledoc")))
