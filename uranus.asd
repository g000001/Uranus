(in-package :cl-user)

(asdf:defsystem :URANUS
  :name "Uranus"
  :description "Uranus is an extension of Prolog written in Common Lisp and has the syntax of Lisp."
  :version "V-19.8"
  :serial t
  :components ((:FILE "package")
               (:FILE "decl")
               (:FILE "defs")
               (:FILE "lib")
               (:FILE "kernel")  
               (:FILE "amuse")
               (:FILE "systempred")
               (:FILE "lispfunctions") 
               (:FILE "stepper")
               (:FILE "trace")
               (:FILE "td")
               (:FILE "readin")))