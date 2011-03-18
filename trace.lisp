;;; -*- Mode:Lisp; Syntax: Common-lisp;  -*-
;;; This file is "Trace.lisp".
;;; Uranus Tracer System
;;; for Zetalisp
;;; 1984/05/11
;;; by H. Nakashima

(in-package :uranus)

(DEFUN Trace-fetch-value (X &OPTIONAL ($SUBST NIL) (FETCH-LEVEL 0.))
   (COND ((ASSIGNED X $SUBST)
          (Trace-fetch-value
           (CADR @FETCHED-VALUE)
           (CDDR @FETCHED-VALUE)
           FETCH-LEVEL))
         ((varp x) (cond ((EQ (CAR $SUBST) '||) X)
			 ($subst 
			  (absolute-variable-form x $subst)
			  )
			 (T X)))
         ((ATOM X) X)
         ((ZEROP (setq fetch-level (1- FETCH-LEVEL))) 'URANUS-USER:?)
         (T (CONS (Trace-fetch-value (CAR X) $SUBST FETCH-LEVEL)
                  (Trace-fetch-value (CDR X) $SUBST FETCH-LEVEL)))))

(DEFUN TRACE-RESULT (RESULT $FORM $SUBST $LEVEL)
   (PRINT '=)
   (PRIN1 $LEVEL)
   (prin1 (Trace-fetch-value (AND RESULT $FORM) $SUBST @PRINTLEVEL)))

(DEFUN TRACE-ENTRY ($FORM $SUBST $LEVEL)
   (PRINT $LEVEL)
   (prin1 (Trace-fetch-value $FORM $SUBST @PRINTLEVEL)))

(DEFUN R@PAUSE NIL
   (if @attention
       (progn
	 (r@push-continuation (ncons @form) @old-subst)	;save the current goal
	 (setq @form '(URANUS-USER:ATTENTION))		;and change it to ATTENTION
	 (setq @definitions (r@getdef @form @unseen-world))
	 (setq @negations (pop @definitions))
	 (setq @attention nil)			;and reset flags
	 (setq @debug (or @debug1 @step @traceall @trace))))
   (PUSH (LIST @LEVEL @FORM @OLD-SUBST) @BACKTRACE)
   (AND (OR @STEP (MEMQ (CAR @FORM) @SELECTSTEP))(URANUS-STEP))
   (AND (OR @TRACEALL (MEMQ (CAR @FORM) @TRACE))
        (TRACE-ENTRY @FORM @OLD-SUBST @LEVEL)))

(DEFUN R@PAUSE2 (RESULT)
   (OR RESULT
       (PUSH (LIST @LEVEL (list '$FAIL$ (car @FORM)) @OLD-SUBST)
             @BACKTRACE))
   (AND (OR @TRACEALL
            (MEMQ (CAR @FORM) @TRACE)
            (AND (OR @STEP (MEMQ (CAR @FORM) @SELECTSTEP))
                 (>= @STEPLEVEL @LEVEL)))
        (TRACE-RESULT RESULT @FORM @OLD-SUBST @LEVEL)))


