;;; -*- Mode:Lisp; Syntax: Common-lisp; Package: URA; -*- 
;;; This file is "Stepper.lisp".
;;; Uranus Stepper 
;;; 1984/05/11 
;;; by Hideyuki Nakashima

(in-package :uranus)

#+symbolics
(DEFUN URANUS-STEP NIL
   (COND ((or @attention @error-or-attention
	      (AND (OR @STEP (MEMQ (CAR @FORM) @SELECTSTEP))
		   (>= @STEPLEVEL @LEVEL)))
          (SETQ @STEPLEVEL 99999.)
          (TRACE-entry @FORM @OLD-SUBST @LEVEL)
          (DO nil
              (NIL)
	    (case (zl:fquery
		     '(:list-choices nil
		       :fresh-line nil
		       :choices
		       (((all "All") #\a)
			((bt  "Back trace") #\b)
			((continue "Continue") #\c #\ ) ;change sp to " " for kcl
			((finish "Finish") #\f)
			((go "Go") #\g)
			((level "Level of printing: ") #\l)
			((next "Next alternative") #\n)
			((pp "Pretty print the goal") #\p)
			((quit "Quit") #\q)
			((select "Select step ") #\s)
			((up "Up") #\u)
			((execute "Execute: ") #\x)
			((lparen "Predicate: ") #\())
		       :help-function  step@help)
		     " S: ")
	      (all (SETQ @STEP T @debug t))
	      (bt (PPRINT (p@BACKTRACE @PRINTLEVEL)))
	      (continue (RETURN NIL))
	      (finish (RETURN (SETQ @STEP NIL @debug (or @step @trace @traceall))))
	      (go  (RETURN (SETQ @STEPLEVEL @LEVEL)))
	      (level (SETQ @PRINTLEVEL (Read)))
	      (next (return (r@fail)))
	      (pp (PPRINT (Trace-fetch-value @FORM @OLD-SUBST)))
	      (quit (THROW :URANUSLOOP NIL))
	      (select (SETQ @STEP NIL)
		(PUSH (RIND "Predicate-name:") @SELECTSTEP))
	      (up  (RETURN (SETQ @STEPLEVEL (1- @LEVEL))))
	      (execute (LET ((@SELECTSTEP NIL) (@STEP NIL))
		      (EXECUTE (Read))))
	      (lparen (LET ((@SELECTSTEP NIL) (@STEP NIL))
			(funcall standard-input ':untyi #\()
			(EXECUTE (read nil)))))))))

#-symbolics
(DEFUN URANUS-STEP NIL
  (COND ((or @attention @error-or-attention
	      (AND (OR @STEP (MEMQ (CAR @FORM) @SELECTSTEP))
		   (>= @STEPLEVEL @LEVEL)))
          (SETQ @STEPLEVEL 99999.)
          (TRACE-entry @FORM @OLD-SUBST @LEVEL)
          (DO ((key (read-from-window #\-) (read-from-window #\-)))
              (NIL)
	    (case key
	      (uranus-user::? (step@help *terminal-io*))
	      (uranus-user::a (SETQ @STEP T @debug t))
	      (uranus-user::b (PPRINT (p@BACKTRACE @PRINTLEVEL)))
	      (uranus-user::c (RETURN NIL))
	      (uranus-user::f (RETURN (SETQ @STEP NIL @debug (or @step @trace @traceall))))
	      (uranus-user::g (RETURN (SETQ @STEPLEVEL @LEVEL)))
	      (uranus-user::l (SETQ @PRINTLEVEL (Read *terminal-io*)))
	      (uranus-user::n (return (r@fail)))
	      (uranus-user::p (PPRINT (Trace-fetch-value @FORM @OLD-SUBST)))
	      (uranus-user::q (THROW :URANUSLOOP NIL))
	      (uranus-user::s (SETQ @STEP NIL)
		(PUSH (RIND "Predicate-name:") @SELECTSTEP))
	      (uranus-user::u (RETURN (SETQ @STEPLEVEL (1- @LEVEL))))
	      (uranus-user::x (LET ((@SELECTSTEP NIL) (@STEP NIL))
		      (EXECUTE (Read *terminal-io*))))
	      (t (cond ((or (consp key) (td-p key))
			(let ((@selectstep nil) (@step nil))
			  (execute key)))
			    (t (format t "Unknown command.  Try ?~%"))))
		      )))))

(defun step@help (stream)
       (format stream
	       "~%The commands are one of the following characters:
A: Stop at all predicates;
B: Back trace (effective only in the debugging mode);
C: Continue one step (a space will do);
F: Finish without stopping;
G: Go (execute this goal only);
L: change the Level of printing;
N: fail this goal and try the Next alternative;
P: Pretty print the current goal;
Q: Quit;
S: Select predicates to stop;
U: Up one level;
X: eXecute a predicate; or
(pred ...).~%"))

