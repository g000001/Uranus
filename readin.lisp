;;; -*- Mode:Lisp; Syntax: Common-lisp;  -*-
;;; Uranus Read-in functions for Common Lisp
;;; (c) H. Nakashima and S. Tomura

(in-package :uranus)

;;; Term Description Forms:
;;;
;;; Eager mode: { <term> ! <pred> }
;;; Lazy  mode: [ <term> ! <pred> ]

;;; Short Hands for Term Descriptions:
;;; [ <pred> ] === [ * ! <pred> * ]
;;; EX.  [p 1] === [ * ! (p 1 *) ]


#|(defun uranus-read-\[ (stream list-so-far)
  list-so-far   ;;; to suppress the warning of the compiler
  (do ((x nil) (form (read stream) (read stream)))
      ((eq form '\])
       (make-up-td (nreverse x) :lazy))
    (push form x)))|#

(defun uranus-read-\[ (stream list-so-far)
  (declare (ignore list-so-far))
  (make-up-td (read-delimited-list #\] stream T) :lazy))

(defun make-up-td (x mode)
  (make-td :pred (make-pred-form x)
	   :term (make-term-form x)
	   :mode mode))


(defun make-pred-form (x)
    (if (eq (second x) '!)
	(third x)
	(if (smember '* x)
	    x
	    (nconc x '(*)))))


(defun make-term-form (x)
  (if (eq (second x) '!)
      (first x)
      '*))


(defun uranus-read-\{ (stream list-so-far)
  (declare (ignore list-so-far))
  (make-up-td (read-delimited-list #\} stream T) :lazy))


(defun smember (x y)
  (cond ((eq x y))
	((atom y) nil)
	((smember x (car y)))
	((smember x (cdr y)))))

;;;   @(cons 1 2) --> [eval (cons 1 2)]

(defun uranus-read-\@ (stream list-so-far)
  list-so-far   ;;; to suppress the warning of the compiler
  (make-td :pred `(eval ,(read stream) *)
	   :term '*
	   :mode :eager))

