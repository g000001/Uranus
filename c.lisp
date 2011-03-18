#+symbolics(in-package 'uranus :use '(cl cl-user) :nicknames '(ura))
#-symbolics(in-package 'uranus :nicknames '(ura))

(defvar uranus@version "V-19.7") ;Uranus version string
(export '(uranus@version) 'uranus)

(load "decl.lisp")

(load "defs")

(in-package 'user)

(load "lib")
(load "kernel")
(load "amuse")
(load "systempred")
(load "lispfunctions")
(load "stepper")
(load "trace")
(load "td")
(load "readin")



#+(and common vax (not kcl))
(defun bye () (exit))

(uranus::uranus)

