;;; -*- Syntax: Common-lisp -*-
#+symbolics(in-package :cl-user)
#-symbolics(in-package :user)

(push 'uranus *features*)

(export '(ON
	   OFF
	   ? ! /] ??? % ||
	   OK
	   compile 
	   amuse
	   assert assertq asserta assertz
	   as aq az aa define
	   deny denya denyz denyq
	   edit
	   standard-world
	   system
	   standard-input
	   standard-output
	   lambda
	   clause
	   macro
	   ATTENTION
	   uranus-system-code) 
	#+symbolics 'cl-user
	#-symbolics 'user)

;;; to amuse
(export '($$$ ? n b p pp i in ib it k d e c f v fn r
	      ra r1 r2 r3 var z s sc q o pop st
	      stack top level x l last li ri bi bo u ?
	      member append nil
	      * *ELEMENT *REST *TOP *REST *any *a *x *y *z)
	#+symbolics 'cl-user
	#-symbolics 'user)

#-symbolics(in-package 'time)
#-symbolics
(export '(microsecond-time
	get-universal-time
	fixnum-microsecond-time
	    ) 'time)

#-symbolics
(set-dispatch-macro-character #\# #\^
  (get-dispatch-macro-character #\# #\\))

