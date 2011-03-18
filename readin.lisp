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
  (DECLARE (IGNORE LIST-SO-FAR))
  (make-up-td (READ-DELIMITED-LIST #\] STREAM T) :lazy))

(defun make-up-td (x mode)
  (make-td :pred (make-pred-form x)
	   :term (make-term-form x)
	   :mode mode))

#|(defun make-pred-form (x)
    (if (eq (second x) '!)
	(third x)
	(if (smember '* x)
	    x
	    (nconc x '(*)))))|#

(defun make-pred-form (x)
    (if (STRING= (second x) '!)
	(third x)
	(if (smember '* x)
	    x
	    (nconc x '(*)))))

#|(defun make-term-form (x)
  (if (eq (second x) '!)
      (first x)
      '*))|#

(defun make-term-form (x)
  (if (STRING= (second x) '!)
      (first x)
      '*))

(defun uranus-read-\{ (stream list-so-far)
  (DECLARE (IGNORE LIST-SO-FAR))
  (make-up-td (READ-DELIMITED-LIST #\} STREAM T) :lazy))

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

(let ((*readtable* uranus-readtable))
  (set-macro-character       #\[ #'uranus-read-\[)
  #|(set-macro-character       #\] #'(lambda (x y) '\]))|#
  (set-macro-character #\] (get-macro-character #\) nil))
  (set-macro-character       #\{ #'uranus-read-\{ )
  #|(set-macro-character       #\} #'(lambda (x y) '\}))|#
  (set-macro-character #\} (get-macro-character #\) nil))
  (set-macro-character       #\@ #'uranus-read-\@ ))

#|||
\(DEFVAR *PREVIOUS-READTABLES* ())

\(defun %enable-uranus-syntax ()
  (push *readtable*
        *previous-readtables*)
  (setq *readtable* (copy-readtable))
  (set-macro-character       #\[ #'uranus-read-\[)
  (set-macro-character #\] (get-macro-character #\) nil))
  #|(set-macro-character       #\] #'(lambda (x y)
                                     (DECLARE (IGNORE X Y))
                                     '\]))|#
  (set-macro-character       #\{ #'uranus-read-\{ )
  (set-macro-character #\} (get-macro-character #\) nil))
  #|(set-macro-character       #\} #'(lambda (x y) 
                                     (DECLARE (IGNORE X Y))
                                     '\}))|#
  (set-macro-character       #\@ #'uranus-read-\@ )
  (values))

\(defun %disable-uranus-syntax ()
  "Internal function used to restore previous readtable." 
  (if *previous-readtables*
    (setq *readtable* (pop *previous-readtables*))
    (setq *readtable* (copy-readtable nil)))
  (values))

\(defmacro enable-uranus-syntax ()
  "Enable CL-URANUS reader syntax."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (%enable-uranus-syntax)))

\(defmacro disable-uranus-syntax ()
  "Restore readtable which was active before last call to
ENABLE-URANUS-SYNTAX. If there was no such call, the standard
readtable is used."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (%disable-uranus-syntax)))

|||#
