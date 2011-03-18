;;; -*-  Mode: LISP; Syntax: Common-lisp -*-
;;; This file is "td.lisp".
;;; Uranus Interpreter
;;; for Common Lisp
;;; 1984/05/11
;;; (c) H. Nakashima and S. Tomura

(in-package 'uranus)

(defstruct (td (:print-function td-printer))
  (level 0)
  term
  pred
  mode)

#+s3600
(defstruct (td :named 
	       :predicate
	       (:print "~VQ"
		       td
		       #'print-td)
	       :conc-name
	       )
  (level 0)
  term
  pred
  mode)

#+kcl (defun td-p (x) (typep x 'td))

(defun td-printer (td str level)
  (declare (ignore level))
  (print-td td str))

(defun print-td (td str)
  (do ((i (td-level td) (1- i)))
      ((equal i 0))
      (princ "'" str))
  (if (eq (td-mode td) :eager)
      (print-eager-td (td-print-form td) str)
      (print-lazy-td (td-print-form td) str)))

(defun td-print-form (td)
  (if (eq (td-term td) 'user::*)
      (if (and (listp (td-pred td))
	       (eq (car (last (td-pred td))) 'user::*))
	  (strip-last (td-pred td))
	  (td-pred td))
      `(,(td-term td) user::! ,(td-pred td))))

(defun strip-last (l)
  (cond ((atom l) nil)
	((null (cdr l)) nil)
	(t (cons (car l) (strip-last (cdr l))))))

(defun print-eager-td (td str)
  (if (and (listp td) (eq (car td) 'eval))
      (format t "/@~a" (second td))
      (progn (princ "{" str)
	     (print-td-rest td "}" str))))

(defun print-lazy-td (td str)
  (princ "[" str)
  (print-td-rest td "]" str))

(defun print-td-rest (td endsym str)
  (cond ((null td) (princ endsym str))
	((atom td)
	 (princ ". " str)
	 (prin1 td str) (princ endsym str))
	((null (cdr td))
	 (prin1 (car td) str)
	 (princ endsym str))
	(t (prin1 (car td) str)
	 (princ " " str)
	 (print-td-rest (cdr td) endsym str))))
