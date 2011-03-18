(IN-PACKAGE :URANUS)
(defvar uranus@version "V-19.8") ;Uranus version string

(LET ((*DEFAULT-PATHNAME-DEFAULTS* #P"/home/mc/Desktop/uranus/uranus/"))
  (load "decl.lisp")
  (load "defs")
  (load "lib")
  (load "kernel")
  (load "amuse")
  (load "systempred")
  (load "lispfunctions") 
  (load "stepper")
  (load "trace")
  (load "td")
  (load "readin"))

#+kcl
(defun si:top-level ()
  (format t "Uranus ~a on KCl~%" uranus:uranus@version)
  (uranus::uranus))

#+akcl
(defun si:top-level ()
  (format t "Uranus ~a on akcl~%" uranus:uranus@version)
  (uranus::uranus))

#+(and common vax (not kcl))
(defun bye () (exit))

#+(and common vax (not kcl))
(defun save-uranus-system (filename)
  (suspend filename)
  (format t "Uranus ~a on VaxLisp~%" uranus:uranus@version)
  (uranus))

#+(and common vax (not kcl))
(setf *gc-verbose* nil)

#+(and common vax (not kcl))
(format t "

When you have finished loading this file, type
     (save-uranus-system \"uranus.sus\")
to create the system Uranus.

")

#+lucid
(defun top-level ()
  (format t "Uranus ~a on Lucid~%" uranus:uranus@version)
  (uranus:uranus))

#+lucid
(defun save-uranus ()
  (disksave "uranus" :restart-function #'top-level))
;  (disksave "uranus" :restart-function #'top-level :full-gc t))

#+lusid
(format t "

When you have finished loading this file, type
     (save-uranus)
to create the system Uranus.

")

#+lucid
(setq *gc-silence* t)