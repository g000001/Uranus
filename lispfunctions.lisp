;;; -*- Mode: Lisp; Syntax: Common-lisp; -*-
;;; This file is "Lispfunctions".
;;; Set up the interface to Lisp functions.
;;; by Hideyuki Nakashima

(in-package :uranus)

;;; :LISP-PREDICATE indicates that no extra argument should be added in Uranus.
;;;   They succeed only when the corresponding lisp functions return non-NIL.
;;; :LISP-PROCEDURE indicates that it should always succeed regardless of the value.
;;;   :LISP-PROCEDURE should be a subset of :LISP-PREDICATE

(defun init-lispfunctions ()

  (MAPC #'(LAMBDA (X) (setf (get X :LISP-PREDICATE) T))
	'(alphalessp aset atom call close defun
          dribble-start dribble-end ed fillarray format
	  fquery greaterp lessp #+symbolics zl:login #+symbolics zl:logout memq null
	  numberp setf setq #+LISPM zl:send setsyntax symbolp tab
	  viewf yes-or-no-p zerop > < >= <=))

#+symbolics
  (MAPC  #'(LAMBDA (X) (setf (get X :LISP-PROCEDURE) T))
	'(close defun ed format
		zl:login zl:logout setf zl:send setq zl:viewf))

#-symbolics
  (MAPC  #'(LAMBDA (X) (setf (get X :LISP-PROCEDURE) T))
	'(close defun ed format
		setf send setq))

;;; Interface to AMUSE
  (MAPC  #'(LAMBDA (X) (setf (get X :LISP-PREDICATE) T))
	 '(
;	   ec:bi ec:e ec:f ec:fn ec:in ec:ib ec:it
;	   ec:level ec:r ec:ra ec:sc ec:u ec:var
	   amuse:putfile)))

