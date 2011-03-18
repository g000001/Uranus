;;; -*- Mode: Lisp; Syntax: Common-lisp -*-
;;; Libralies for KCl/Symbolics
;;; (c) H. Nakashima

(in-package 'uranus)

#+lucid (proclaim '(optimize speed (safety 0)))

(defun rind (prmpt) (princ prmpt standard-input) (read standard-input))

#-symbolics
(defun tyi (&optional (stream *terminal-io*))
  (read-char stream))

#-symbolics
(defun untyi (&optional (stream *terminal-io*))
  (unread-char stream))

#-symbolics
(defun tyipeek (&optional (stream *terminal-io*))
  (peek-char nil stream))

#-symbolics
(defun tyo (x &optional (stream *terminal-io*))
  (read-char x stream))

#-symbolics(defmacro lessp (x y) `(< ,x ,y))
#-symbolics(defmacro greaterp (x y) `(> ,x ,y))

(defmacro putprop (s v ind)
  `(setf (get ,s ,ind)  ,v))
(defmacro  defprop (s v ind)
  `(putprop ',s ',v ',ind))
(defmacro ncons (x) `(cons ,x nil))

#-symbolics
(defmacro get-pname (s)
  `(if ,s (symbol-name ,s) "NIL"))

(defmacro aset (v a n)
  `(setf (aref ,a ,n) ,v))

#+kcl
 (defmacro memq (x a-list)
   `(member ,x ,a-list :test #'eq))

#+kcl
  (defmacro delq (x y &optional (n 100)) 
   `(delete ,x ,y :test #'eq :count ,n))

#-symbolics
(defun remq (x y &optional (n 100))
  (remove x y :test #'eq :count n))
#-symbolics
(defmacro fsymeval (x)
  `(symbol-function ,x))

#-symbolics
(defmacro flatsize (x)
  `(cond (t 1)))

(defmacro pathtosym (x)
    `(intern (namestring ,x)))

#+kcl
(defmacro user::string-append (&body s)
  `(concatenate 'string . ,s))

#+symbolics					;Rel 7
(progn
  (remprop 'and 'gprint::formatter)
  (remprop 'or 'gprint::formatter)
  )

#+lucid
(defun special-form-p (x) nil)
