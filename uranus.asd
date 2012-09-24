(in-package :cl-user)

(asdf:defsystem :uranus
  :name "Uranus"
  :description "Uranus is an extension of Prolog written in Common Lisp and has the syntax of Lisp."
  :version "V-19.8"
  :serial t
  :components ((:file "package")
               (:file "decl")
               (:file "defs")
               (:file "lib")
               (:file "kernel")  
               (:file "amuse")
               (:file "systempred")
               (:file "lispfunctions") 
               (:file "stepper")
               (:file "trace")
               (:file "td")
               (:file "readin")
               (:file "readtable")))
