;;; -*- Mode: Lisp; Package:Uranus; Base: 10. -*-
;;; This file is "Kernel.lisp".
;;; Uranus Interpreter
;;; for Common Lisp
;;; 1984/05/11
;;; (c) H. Nakashima and S. Tomura
#+symbolics(in-package 'uranus :use '(cl cl-user) :nicknames '(ura))
(in-package :uranus)
#+lucid (proclaim '(optimize speed (safety 0)))


(DEFUN VERSION NIL (PRINC uranus@version)) ;;; changed

;;; terminal-io is a instance of Uranus-listener.
;;; If Uranus-listener becomes a frame, the name must be changed.

#-symbolics					;See window.lisp for symbolics
(defun uranus-initial-function ()
  (init)
  (do () (nil) (pgo)))

(defun uranus ()
  (let ((terminal-input *terminal-io*)
	(terminal-output *terminal-io*)
	(standard-input *terminal-io*)
	(standard-output *terminal-io*))
    (IN-PACKAGE :URANUS-USER)
    (init)
    (pgo)))

(DEFUN INIT ()
  (setq line-length 80.)
  (SETQ @URANUS-WORLD '(URANUS-USER::standard-WORLD))  
  (setq @basic-world '(uranus-user::standard-world))
  (SETQ @PRINTLEVEL 99.)
  (setq @level 0.)			;necessary for printing the result
  (SETQ @TRACE NIL)
  (SETQ @TRACEALL NIL)
  (SETQ @STEP NIL)
  (setq @error-or-attention nil)
  (SETQ @STEPLEVEL 99999.)
  (SETQ @SELECTSTEP NIL)
  (SETQ @DEBUG1 NIL)
  #+symbolics(fs:set-default-pathname "uranus-init.ura" @default-pathname)
  (INIT-SYSTEM-CODE)
  #+symbolics (zl:send *terminal-io* ':add-asynchronous-character "#^g" #'kbd-attention)
  #+symbolics (zl:send *terminal-io* ':add-asynchronous-character #\suspend #'kbd-attention)
  #+(and vax (not kcl)) ;;; vaxlisp
  (bind-keyboard-function "#\^c" #'(lambda () (funcall attention-handler)))
  )

;;; Utilisp compatible attention handling functions for kcl.

#+symbolics
(DEFUN kbd-attention (CHAR &OPTIONAL IGNORE &AUX P)
  (tv:KBD-ESC-CLEAR NIL)			;Forget chars typed before "CTRL-abort", 
						;even those inside window's iob
  (AND (SETQ P TV:SELECTED-WINDOW)	;Find process to be hacked
       (SETQ P (zl:send P ':PROCESS))
       (case CHAR
	 (("#^G" #\suspend)
	  ;;; (funcall p ':interrupt . &) means (send  p ':interrupt . &).
	  (zl:PROCESS-RUN-FUNCTION '(:NAME "Attention" :PRIORITY 40.) P ':INTERRUPT 
				'call-attention	char))
	 )))

#+symbolics
(defun call-attention (char) char (funcall attention-handler))

#+kcl
;;; The code borrowed from kcl/lsp/top.lsp
(defun system::terminal-interrupt (correctablep)
  (let ((*break-enable* t))
    (if correctablep 
	(funcall attention-handler)
      (error "Console interrupt -- cannot continue."))))

#+kcl (setf system::*quit-tag* :uranusloop)

#+(and vax (not kcl)) ;;; vaxlisp
(defun uranus-error-handler (function-name
			     error-signaling-function
			     &rest args)
  (format *debug-io* "~&ERROR: ")
  (apply #'format *debug-io* args)
  (format *debug-io* 
	  "~&       Lisp error signalled by ~s" function-name)
  (if (y-or-n-p "Exit to Uranus toplevel?")
      (throw :uranusloop nil)
  (apply #'universal-error-handler
	 function-name
	 error-signaling-function
	 args)))

#+(and vax (not kcl)) ;;; vaxlisp
(eval-when (eval load)
	   (setf uranus-user::*universal-error-handler* 'uranus-error-handler))

#+(and vax (not kcl)) ;;; vaxlisp
(eval-when (eval load)
	   (setf uranus-user::*top-level-prompt* "Type (uranus) to continue Uranus :"))

#+(and vax (not kcl)) ;;; vaxlisp
(eval-when (eval load)
	   (setf (symbol-function 'uranus-user::uranus)
		 (symbol-function 'uranus::uranus)))


(defmacro newsubst (&rest sb)
  (cond (sb `(cons (ncons ,(car sb)) @uranus-world))
	(t `(cons (ncons nil) @uranus-world))))


(DEFMACRO VARP (X) 
  `(and (symbolp ,x)
	(not (eq ,x 'uranus-user::||))
	(char= (aref (symbol-name ,x) 0) #\*))
  )


(defun lazy-expat-p (x)
  (and (td-p x) (eq (td-mode x) :lazy)))

(defun expat-p (x)
  (and (td-p x) (eq (td-mode x) :eager)))


(DEFUN REPORT-ERROR (MES &OPTIONAL (AT ""))
  (r@push-continuation `((error ,mes ,at)) @OLD-SUBST)
  (r@succeed)
  (throw 'errset `(error ,mes ,at)))

;;; Uranus standard error handler routine.
(DEFUN URANUS-BREAK NIL
  (LET ((STANDARD-OUTPUT terminal-output) (@error-or-attention T)) (URANUS-STEP)))

;;; Uranus standard attention handler.
(DEFUN ATTENTION NIL
  (setq @step t @debug t)
  )

(defun uranus-user::c nil (r@succeed) (throw :refuteloop t))

(defun uranus-user::s nil
  (setq @step t @debug t)
  (r@succeed) (throw :refuteloop t))

#-symbolics
(defun read-from-window (prmpt)
  (terpri terminal-input)
  (write-char prmpt terminal-input)
  (force-output terminal-input)
  (read terminal-input))

(DEFUN PGO NIL
  (CATCH :LISP
	 (LET ((ATTENTION-handler
		#'(LAMBDA nil (throw :uranusloop nil)))
	       (*readtable* uranus-readtable))
	      (do () (nil)
		  (catch :uranusloop
			 (DO ((INPUT (read-from-window #\:)
				     (read-from-window #\:))
			      (@BACKTRACE NIL NIL)
			      (@uranus-world @basic-world @basic-world)
			      (@step nil nil))
			     (NIL)
			     (CATCH :TOP
				    (EXECUTE INPUT)
				     )))))))

(defun toplevel-execute (item &optional (printp t))	;used from ZWEI
  (let ((@uranus-world @basic-world))
    (execute item printp)))


(DEFUN EXECUTE (ITEM &optional (printp t))
  (LET ((@DEBUG nil))
    (update-debug-flag)
    (CATCH :URANUSLOOP			
      (LET ((ATTENTION-HANDLER (FUNCTION ATTENTION)) 
            ($SUBST (NEWSUBST))			
            (@undolist nil) )
        (let ((result (if (td-p item) item
                          (refute item $subst) )))
          (AND printp
               (pprint (FETCHVALUE RESULT $SUBST @PRINTLEVEL)) )
          (SETQ @LAST-RESULT (CONS RESULT $SUBST))
          (FETCHVALUE RESULT $SUBST @PRINTLEVEL) )))
    (setf (get 'LAST-INPUT (if (atom @uranus-world) @uranus-world (CAR @URANUS-WORLD)))
          (NCONS (NCONS (NCONS ITEM))) )))

;;; Result is a interfece to Lisp.  (result <form>) returns the result.

(defun uranus-user::result (item)
  (catch :uranusloop			
    (let ((@debug nil)
	  (attention-handler (function attention)) 
	  ($subst (newsubst))			
	  (@undolist nil))
      (let ((result (if (td-p item) item
		      (refute item $subst))))
	(fetchvalue result $subst)))))


(defun update-debug-flag ()
       (setq @debug
	(OR @TRACEALL @TRACE @STEP @selectstep @DEBUG1)))

(DEFUN REFUTE (@FORM &OPTIONAL (@OLD-SUBST (NEWSUBST))
                     (@LEVEL (1+ @level))
                     (@CUE NIL) (@STACK NIL))
  (LET* ((@GOAL @FORM) (@FINAL-UNDO-POINT @UNDOLIST)
	 (@NEW-SUBST (NEWSUBST))
	 (@CLAUSE NIL)
	 (@unseen-world @uranus-world)			;must be bound before R@GETDEF
	 (@DEFINITIONS (R@GETDEF @FORM @uranus-world))
	 (@negations (pop @definitions))
	 (@UNDO-POINT @UNDOLIST)
	 (@mother @stack)
	 (@father @stack)
	 (@world-at-entrance @uranus-world))
   (refute-one-loop)))

(defun REFUTE@ONE NIL
  ;;; AND-process phase
  (if @CLAUSE (r@push-continuation @CLAUSE @OLD-SUBST))
  (catch 'errset
    (do nil
        ((COND ((listp @form)
                (cond((varp (car @form))
		      (setq @form `(,(fetchvalue (car @form) @old-subst)
				     . ,(cdr @form)))
		      (setq @definitions (r@getdef @form @uranus-world))
		      (setq @negations (pop @definitions))
		      t)

;;; The executable pattern at the top-level should be executed twice as:
;;;   [list print a] --> (print a) --> a
;;; Nevertheless, [times 2 3] --> 6  at the top-level is convenient.
;;; Therefore, the following codes remains to be comments for a while.
;;; The toplevel print routine takes care of the value.
;;;
#|
;		     ((td-p @form)
;		      (setq @form (fetchvalue @form @old-subst))
;		      (setq @definitions (r@getdef @form @uranus-world))
;		      (setq @negations (pop @definitions))
;		      t)
|#
		     (t)))
               ((varp @form)
                (setq @form (fetch @form @old-subst))
		(setq @old-subst @fetched-subst)
                (cond
		  ((td-p @form)
		   (setq @form (fetchvalue @form @old-subst))
		   (setq @definitions (r@getdef @form @uranus-world))
		   (setq @negations (pop @definitions))
		   t)
		  ((listp @form)
		   (setq @definitions (r@getdef @form @uranus-world))
		   (setq @negations (pop @definitions)))
		  (t (report-error "ILLEGAL FORM" @form) t)))
               (t(report-error "ILLEGAL FORM" @form) t))))
    (AND @DEBUG (R@PAUSE))
    ;;; OR-process phase
    (DO (($definitions @definitions))		;switch for error/fail
	(nil)
      (cond
	((atom @definitions)			;nil=no def. t=intermediate stage --v
	 (cond ((setq @definitions (r@getdef @form @unseen-world))
		(setq $definitions t)
		(setq @negations (append (pop @definitions) @negations)))
	       ;;; System predicates are executed under the AND-process.
	       ((SYSTEM (CAR @FORM) (CDR @FORM) @OLD-SUBST $definitions)
		(return (R@SUCCEED)))
	       (T (AND @DEBUG (R@PAUSE2 NIL)) (return (R@FAIL)))))
	((null (car @definitions))
	 (return (progn (r@fail) nil)))
	((compiled-code-p (car @definitions))
	 (if(execute-compiled-code (pop @definitions)) (return t)))
	((UNIFY (CAAR @DEFINITIONS) @NEW-SUBST (CDR @FORM) @OLD-SUBST)
	 (setq @father @stack)			; save @stack into @father
	 (COND ((CaDR @DEFINITIONS)		;If the rest does not begin with NIL
		(r@push-alternatives @FORM (CDR @DEFINITIONS) @negations @old-subst)
		(setq @mother @stack))
	       ((cdr @definitions))		;Rest begin with NIL. No more alternative search.
	       (@unseen-world
		(r@push-alternatives @FORM	;flag to error/undef.             --^
				     t
				     @negations
				     @old-subst)
		(setq @mother @stack))
	       (@DEBUG (r@push-alternatives `($FAIL$ , @FORM) NIL NIL @old-subst)))
	 (AND @DEBUG (R@PAUSE2 T))
	 ;;; tail recursive loop
	 (RETURN (COND ((SETQ @CLAUSE (CDR (CAR @DEFINITIONS)))
			(if @negations (r@push-continuation nil @OLD-SUBST))
			(SETQ @FORM (POP @CLAUSE))
			(setq @DEFINITIONS (R@GETDEF @FORM @uranus-world))
			(setq @negations (pop @definitions))
			(SETQ @LEVEL (1+ @LEVEL))
			(SETQ @UNDO-POINT @UNDOLIST)
			(SETQ @OLD-SUBST @NEW-SUBST)
			(SETQ @NEW-SUBST (NEWSUBST))
			T)
		       (@negations
			(if (r@check-negations @form @old-subst @negations)
			    (r@succeed)
			    (r@fail)))
		       ((R@SUCCEED)))))
	(T (UNDO @UNDO-POINT) (POP @DEFINITIONS))))))


;;; Compiled code structure
(defun compiled-code-p (f) 
      (typep f 'compiled-function))
(defun execute-compiled-code (f)
  (funcall f))


(defun refute-one-loop ()
  (catch :refute (do nil (nil)
		     (catch :refuteloop
		       (refute@one)))))


(DEFUN REFUTE@N (@CLAUSE @OLD-SUBST @LEVEL)
  (cond (@clause
         (LET* ((@FORM (POP @CLAUSE))
		(@cue nil) (@stack nil)
		(@GOAL @FORM) (@FINAL-UNDO-POINT @UNDOLIST)
		(@unseen-world @uranus-world)
		(@DEFINITIONS (R@GETDEF @FORM @uranus-world)) 
		(@negations (pop @definitions))
		(@UNDO-POINT @UNDOLIST)
		(@NEW-SUBST (NEWSUBST)) 
		(@world-at-entrance @uranus-world))
	   (refute-one-loop)))
	(t)))


(defun value-form (value) (cadr value))
(defun value-subst(value) (cddr value))
(defun value-world(value) (cdddr value))


(DEFUN ASSIGNED (X subst) (SETQ @FETCHED-VALUE (ASSoc X (CDaR subst) :test #'eq)))


;;;  value === <form, subst>
;;;  subst === ((id . {var:value}) .  world)

;;; The value of setf is different in Zetalisp and Commonlisp.

(DEFUN LINK (X XSUBST Y YSUBST)
  (OR (AND (EQ (car XSUBST) (car YSUBST)) (EQ X Y))
      (PUSH (CONS (RPLACD (car XSUBST)
			  (CONS (CONS X (CONS Y YSUBST))
				(CDaR XSUBST)))
                  (CADR (car XSUBST)))
            @UNDOLIST)))

(DEFUN FETCH (X subst)
   (COND ((ASSIGNED X subst)
          (fetch (value-form @FETCHED-VALUE) (value-subst @FETCHED-VALUE)))
         (T (SETQ @FETCHED-SUBST subst) X)))


(DEFUN UNDO (UP)
  (DO ()
      ((EQ @UNDOLIST UP))
    ;; (DELETE (CDAR @UNDOLIST) (CAAR @UNDOLIST) :test #'eq :count 1)
    (SETF (CAAR @UNDOLIST)
          (DELETE (CDAR @UNDOLIST) (CAAR @UNDOLIST) :test #'eq :count 1))
    ;; (DELq (CDAR @UNDOLIST) (CAAR @UNDOLIST) 1)
    (POP @UNDOLIST)))

;;;
;;; Lambda form: ((lambda <var> <body>) <arg>)
;;; The scope of the lambda form variable is within its body. 
;;; CAUTION : Lambda form does not backtrack.

(defmacro lambda-bind (var vsubst arg asubst body)
  `(let ((@bind nil))
     (lambda-link ,var ,vsubst ,arg ,asubst)
     (prog1 ,body 
	    #|(delq @bind (car ,vsubst))|#
            (SETF (car ,vsubst)
                  (delq @bind (car ,vsubst))))))

(defun lambda-form (var body $args $subst)
  (lambda-bind var $subst $args $subst
	       (refute body $subst (1+ @level))))

(defun lambda-link (var vsubst arg asubst)
  (setf (cdar vsubst) (cons (setq @bind `(,var ,arg . ,asubst))
			    (cdar vsubst))))


(DEFUN UNIFY (X XSUBST Y YSUBST)
  (AND (VARP Y)
       (ASSIGNED Y YSUBST)
       (SETQ Y (FETCH (CADR @FETCHED-VALUE) (CDDR @FETCHED-VALUE)))
       (SETQ YSUBST @FETCHED-SUBST))
  (COND ((OR (EQ X 'URANUS-USER::?) (EQ Y 'uranus-user::?)))
	((expat-p x) (!@REFUTE X XSUBST Y YSUBST))
	((expat-p y) (!@REFUTE Y YSUBST X XSUBST))
        ((VARP X)
         (COND ((ASSIGNED X XSUBST)
                (UNIFY (CADR @FETCHED-VALUE) (CDDR @FETCHED-VALUE) Y YSUBST))
               (T (LINK X XSUBST Y YSUBST))))
	((VARP Y) (LINK Y YSUBST X XSUBST))
	((lazy-expat-p x) (!@REFUTE X XSUBST Y YSUBST))
	((lazy-expat-p y) (!@REFUTE Y YSUBST X XSUBST))
	((null x) (null y))			;(listp nil)=T in Symbolics
        ((listp X)
         (COND ((and (EQ (CAR X) 'QUOTE) (equal (length x) 2))
		(unify-q-n (second x) xsubst y ysubst))
	       ((ATOM Y) NIL)
               ((and (EQ (CAR Y) 'QUOTE) (equal (length y) 2))
		(unify-q-n (second y) ysubst x xsubst))
               ((UNIFY (CAR X) XSUBST (CAR Y) YSUBST)
                (UNIFY (CDR X) XSUBST (CDR Y) YSUBST))))
        ((listp Y)
         (COND ((and (EQ (CAR Y) 'QUOTE) (equal (length y) 2))
		(unify-q-n (second y) ysubst x xsubst))
	       (t nil)))
        (T (EQUAL X Y))))

;;; x is quoted
(DEFUN UNIFY-Q-N (X XSUBST Y YSUBST)
  (AND (VARP Y)
       (ASSIGNED Y YSUBST)
       (SETQ Y (FETCH (CADR @FETCHED-VALUE) (CDDR @FETCHED-VALUE)))
       (SETQ YSUBST @FETCHED-SUBST))
  (COND ((EQ X 'uranus-user::?))
	((EQ Y 'uranus-user::?))
	((expat-p y) (!@REFUTE Y YSUBST `',X XSUBST))
        ((VARP X)
         (COND ((ASSIGNED X XSUBST)
                (UNIFY-Q-N (CADR @FETCHED-VALUE) (CDDR @FETCHED-VALUE) Y YSUBST))
               (T (LINK X XSUBST Y YSUBST))))
	((VARP Y) (LINK Y YSUBST `',X XSUBST))
	((lazy-expat-p y) (!@REFUTE Y YSUBST `',X XSUBST))
	((null x) (null y))			;(listp nil)=T in Symbolics
        ((listp X)
         (COND ((ATOM Y) NIL)
               ((and (EQ (CAR Y) 'QUOTE) (equal (length y) 2))
		(unify-q-q (second y) ysubst x xsubst))
               ((UNIFY-Q-N (CAR X) XSUBST (CAR Y) YSUBST)
                (UNIFY-Q-N (CDR X) XSUBST (CDR Y) YSUBST))))
        ((listp Y)
         (COND ((and (EQ (CAR Y) 'QUOTE) (equal (length y) 2))
		(unify-q-q (second y) ysubst x xsubst))
	       (t nil)))
        (T (EQUAL X Y))))


;;; x and y are quoted
(DEFUN unify-q-q (X XSUBST Y YSUBST)
  (AND (VARP Y)
       (ASSIGNED Y YSUBST)
       (SETQ Y (FETCH (CADR @FETCHED-VALUE) (CDDR @FETCHED-VALUE)))
       (SETQ YSUBST @FETCHED-SUBST))
  (COND	((EQ X 'uranus-user::?))
	((EQ Y 'uranus-user::?))
	((VARP X)
         (COND ((ASSIGNED X XSUBST)
                (unify-q-q (CADR @FETCHED-VALUE) (CDDR @FETCHED-VALUE) Y YSUBST))
               (T (LINK X XSUBST Y YSUBST))))
        ((VARP Y) (LINK Y YSUBST X XSUBST))
	((null x) (null y))			;(listp nil)=T in Symbolics
	((listp X)
         (COND ((ATOM Y) NIL)
               ((unify-q-q (CAR X) XSUBST (CAR Y) YSUBST)
                (unify-q-q (CDR X) XSUBST (CDR Y) YSUBST))))
	((and (td-p x) (td-p y))
	 (unify-t-t x xsubst y ysubst))
        (T (EQUAL X Y))))

;;; x is a TD
(DEFUN UNIFY-T-N (X XSUBST Y YSUBST)
  (AND (VARP Y)
       (ASSIGNED Y YSUBST)
       (SETQ Y (FETCH (CADR @FETCHED-VALUE) (CDDR @FETCHED-VALUE)))
       (SETQ YSUBST @FETCHED-SUBST))
  (COND ((EQ Y 'uranus-user::?))
	((and (td-p y) (> (td-level y) 0))
	 (unify-t-t x xsubst y ysubst))
	((expat-p y)
	 (!@REFUTE Y YSUBST X XSUBST))
	((VARP Y) (LINK Y YSUBST X XSUBST))
	((lazy-expat-p y) (!@REFUTE Y YSUBST X XSUBST))
        ((listp Y)
         (COND ((and (EQ (CAR Y) 'QUOTE) (equal (length y) 2))
		(unify-q-q (second y) ysubst x xsubst))
	       (t nil)))
	))

;;; x and y are TD
;;; How shall we define the unification of frozen TDs?
;;; Esp. How shall we treat the special variable "*"?
;;; In this version, the special variable "*" of TD is replaced
;;; by a completely new variable!!!

;;; Currently, the special treatment of "*" is inhibited.
;;; Modes and levels of TDs must be same.
(defun unify-t-t (x xsubst y ysubst)
;  (let ((xnewsubst (newsubst)) (ynewsubst (newsubst)))
						;xsubst maybe = ysubst
;    (lambda-bind 'uranus-user::* xsubst 'uranus-user::* xnewsubst
;		 (lambda-bind 'uranus-user::* ysubst 'uranus-user::** ynewsubst
			      (and 
				(eq (td-mode x) (td-mode y))
				(= (td-level x) (td-level y))
				(unify-q-q (td-pred x) xsubst (td-pred y) ysubst)
				(unify-q-q (td-term x) xsubst (td-term y) ysubst)))
;			      )))

(defun absolute-variable-form (x $subst)
  (intern (concatenate 'string (symbol-name x) 
		       (symbol-name (or (caar $subst)
					(car (rplaca (car $subst)
						     (gensym "_"))))))))

(DEFUN FETCHVALUE (X &OPTIONAL (@SUBST NIL) (FETCH-LEVEL 0.))
  (fetchvalue1 x @subst fetch-level))

(defun fetchvalue1 (x $subst fetch-level)
  (COND ((VARP X)
         (COND ((ASSIGNED X $SUBST)
                (FETCHVALUE1
                 (CADR @FETCHED-VALUE)
                 (CDDR @FETCHED-VALUE)
                 FETCH-LEVEL))
               ((EQ $SUBST @subst) X)
               (T 
		(absolute-variable-form x $subst)
		)))
        ((td-p x)
	 (if (> (td-level x) 0)
	     (make-td :pred (fetchvalue-q1 (td-pred x) $subst fetch-level t)
		      :term (fetchvalue-q1 (td-term x) $subst fetch-level t)
		      :level (1- (td-level x))
		      :mode (td-mode x))
	     (let (($new-subst (newsubst)))
	       (!@REFUTE x $subst '*fail $NEW-SUBST)
	       (fetchvalue1 '*fail $new-subst fetch-level))))
        ((ATOM X) X)
        ((EQ (CAR X) 'QUOTE)
	 (COND ((EQUAL (LENGTH X) 2.)
		(fetchvalue-q (second x) $subst fetch-level))
	       (T X)))
        ((ZEROP (setq fetch-level (1- FETCH-LEVEL))) 'URANUS-USER::?)
        (T (CONS (FETCHVALUE1 (CAR X) $SUBST FETCH-LEVEL)
                 (FETCHVALUE1 (CDR X) $SUBST FETCH-LEVEL)))))

;;;
;;; fetchvalue in Quoted(Objectized) mode
;;;

(DEFUN fetchvalue-q (X &OPTIONAL (@SUBST NIL) (FETCH-LEVEL 0.))
  (fetchvalue-q1 x @subst fetch-level nil))

(defun fetchvalue-q1 (x $subst fetch-level within-td)
  (COND ((VARP X)
         (COND ((and within-td (eq x 'uranus-user::*)) 'uranus-user::*)
	       ((ASSIGNED X $SUBST)
		(fetchvalue-q1
		  (CADR @FETCHED-VALUE)
		  (CDDR @FETCHED-VALUE)
		  FETCH-LEVEL
		  within-td))
               ((EQ $SUBST @subst) X)
               (T
		(absolute-variable-form x $subst)
		)))
        ((td-p x)
	 (make-td :pred (fetchvalue-q1 (td-pred x) $subst fetch-level t)
		  :term (fetchvalue-q1 (td-term x) $subst fetch-level t)
		  :level (td-level x)
		  :mode (td-mode x)))
        ((ATOM X) X)
        ((ZEROP (setq fetch-level (1- FETCH-LEVEL))) 'URANUS-USER::?)
        (T (CONS (fetchvalue-q1 (CAR X) $SUBST FETCH-LEVEL within-td)
                 (fetchvalue-q1 (CDR X) $SUBST FETCH-LEVEL within-td)))))

;;; Objectize is replaced by fetchvalue
#|
(defun objectize (x $subst)
  (cond ((varp x)
         (cond ((assigned x $subst)
                (objectize
                 (cadr @fetched-value)
                 (cddr @fetched-value)))
               (t x)))
        ((td-p x)
	 (if (> (td-level x) 0)
	     (make-td :pred (td-pred x)
		      :term (td-term x)
		      :mode (td-mode x)
		      :level (1- (td-level x)))
	     (let (($new-subst (newsubst)))
	       (!@REFUTE x $subst '*fail $NEW-SUBST)
	       (fetchvalue '*fail $new-subst))))
        ((atom x) x)
        (t (cons (objectize (car x) $subst)
                 (objectize (cdr x) $subst)))))
|#

(defun metize (x $subst)
  (cond ((varp x)
         (cond ((assigned x $subst)
                (metize
                 (cadr @fetched-value)
                 (cddr @fetched-value)))
               (t x)))
        ((td-p x)
	 (make-td :pred (fetchvalue-q1 (td-pred x) $subst 0 t)
		  :term (fetchvalue-q1 (td-term x) $subst 0 t)
		  :mode (td-mode x)
		  :level (1+ (td-level x))))
        ((atom x) x)
        (t (cons (metize (car x) $subst)
                 (metize (cdr x) $subst)))))

;;; Cue manipulations

(defun r@push-continuation ($clause $subst)
       (let ((cue (make-array 7)))
	 (aset $clause       cue 0)
	 (aset $subst        cue 1)
	 (aset @level        cue 2)
	 (aset @father       cue 3)
	 (aset @uranus-world cue 4)
	 (aset @mother       cue 5)
	 (aset `(, @form . , @negations) cue 6)
	 (push cue @cue)))


(defun r@succeed nil
   (cond ((null @cue) 
	  (setq @uranus-world @world-at-entrance)
	  (throw :refute @goal))
         (t (r@pop-continuation))))

(defun r@pop-continuation nil
  (let ((cue (pop @cue)))
    (setq @clause       (aref cue 0))
    (setq @old-subst    (aref cue 1))
    (setq @level        (aref cue 2))
    (setq @father       (aref cue 3))
    (setq @uranus-world (aref cue 4))
    (setq @mother       (aref cue 5))
    (cond (@clause (r@set-forms @clause))
	  ((r@check-negations (car (aref cue 6)) @old-subst (cdr (aref cue 6)))
	   (r@succeed))
	  (t (r@fail))
	  )))

(defun r@set-forms (clause)
  (cond ((varp clause)
	 (if (assigned clause @old-subst)
	     (progn (setq @old-subst (cddr @fetched-value))
		    (r@set-forms (cadr @fetched-value)))
	     (report-error "UNDEFINED VARIABLE" clause)))
	((null clause) (r@succeed))
	(t (setq @clause clause)
	   (setq @form (pop @clause))
	   (setq @undo-point @undolist)
	   (setq @new-subst (newsubst))
	   (setq @definitions (r@getdef @form @uranus-world))
	   (setq @negations (pop @definitions)))))

(defun r@check-negations (form subst negations)
  (do ((up @undolist)
       (ns (newsubst))
       (negs negations (cdr negs)))
      ((null negs) t)
    (and (unify (caar negs) ns (cdr form) subst)
	 (let ((@uranus-world (r@world-diff @uranus-world @unseen-world)))	;Negations must be checked in
	   (refute@n (cdar negs) ns (1+ @level)))	;outer context of positive assertion only.
	 (return nil))
    (undo up)))

(defun r@world-diff (all rest)
  (do ((w nil (cons (pop all) w)))
      ((eq all rest) (nreverse w))))

;;; Alternative manipulations

(defun r@push-alternatives ($form $definitions $negations $subst &optional (vars nil))
       (let ((stack (make-array 13.)))
         (aset $form             stack 0)
	 (aset $definitions      stack 1)
	 (aset $negations        stack 2)
	 (aset $subst            stack 3)
	 (aset @level            stack 4)
	 (aset @cue              stack 5)
	 (aset @undo-point       stack 6)
	 (aset @final-undo-point stack 7)
	 (aset @father           stack 8)
	 (aset @uranus-world     stack 9.)
	 (aset @unseen-world     stack 10.)
	 (aset @mother           stack 11.)
	 (aset vars              stack 12.)
         (push stack @stack))
       )

(defun r@fail nil
  (cond (@stack ;(and @debug (princ "<" terminal-io))
	 (r@pop-alternatives)
	 (undo @undo-point))
	(t (undo @final-undo-point)
	   (setq @uranus-world @world-at-entrance)
	   (throw :refute nil))))


(defun r@pop-alternatives nil
       (let ((stack (pop @stack)))
	 (setq @form             (aref stack 0))
	 (setq @clause nil)
	 (setq @definitions      (aref stack 1))
	 (setq @negations        (aref stack 2))
	 (setq @old-subst        (aref stack 3))
	 (setq @level            (aref stack 4))
	 (setq @cue              (aref stack 5))
	 (setq @undo-point       (aref stack 6))
	 (setq @final-undo-point (aref stack 7))
	 (setq @father           (aref stack 8))
	 (setq @uranus-world     (aref stack 9))
	 (setq @unseen-world     (aref stack 10))
	 (setq @mother           (aref stack 11))
	 (setq @saved-vars       (aref stack 12))))


;;; Now R@clear is not used.
(DEFUN R@CLEAR NIL (SETQ @CUE NIL @STACK NIL))

(DEFUN SYSTEM (NAME @ARGS $SUBST $definitions)
   (LET ((CODE (R@GET-preD-VALUE NAME :CODE)))
    (COND (CODE (LET ((@SUBST (NEWSUBST '||)))
                 (COND ((UNIFY (POP CODE) @SUBST @ARGS $SUBST)
                        (FUNCALL CODE))
                       (T (REPORT-ERROR
                             "ILLEGAL NUMBER OR TYPE OF ARGUMENT(S)"
                             (FETCHVALUE (CONS NAME @ARGS) $SUBST))))))
          ((LISTP NAME) 
	   (cond
	     ;;; lambda form == (lambda <var>  <body))
	     ((eq (car name) 'uranus-user::lambda)
	      (cond((and(= (length name) 3)(varp (second name)))
		    (lambda-form (second name) (third name) @args $subst))
		   (t (report-error "Illegal lambda form" @form))))
	     ;;; clausal form == (clause <formal-args> . <body>)
	     ((eq (car name) 'uranus-user::clause)
	      (cond ((>= (length name) 2)
		     (clause-form (second name)(cddr name) @args $subst))
		    (t (report-error "Illegal clausal form" @form))))
	     (t (report-error "Illegal predicate form" @form))))
          ($definitions nil)
          ((fboundp name)
           (or (CALL-LISP-FUNCTION
		 NAME
		 @ARGS
		 $SUBST
		 (R@GET-PRED-VALUE NAME :LISP-PREDICATE)
		 (or (special-form-p name) (macro-function name))
		 )
	       (r@get-pred-value name :lisp-procedure)))     ;;Lisp-procedure always succeeds
          (@debug1 (REPORT-ERROR "UNDEFINED PREDICATE" @form)))))

;;;
;;; Clausal form: ((clause <formal args> <body>) <actual args>)
;;; The scope of the variables which appears in clausal form are within its form, that is,
;;; local variable.

(defun clause-form (fargs body args a-subst)
  (let((f-subst (newsubst)))
    (cond((unify args a-subst fargs f-subst)
	  (r@push-continuation body f-subst))
	 (t (undo @undo-point) nil))))

#|
(defmacro R@LAMBDA-BIND (X Y) `(LET ((@BIND NIL)) (PROG1 ,Y (DELQ @BIND ,X))))

(DEFUN R@LAMBDA (LAMBDA-FORM $ARGS $SUBST)
   (R@LAMBDA-BIND
      $SUBST
      (REFUTE (CADDR LAMBDA-FORM)
              (R@LAMBDA-LINK (CADR LAMBDA-FORM) $SUBST $ARGS $SUBST)
              (1+ @LEVEL))))

(DEFUN R@LAMBDA-LINK (VAR VAR-SUBST ARGS A-SUBST)
  (prog1 (RPLACD VAR-SUBST
                 (CONS (SETQ @BIND (CONS VAR (CONS ARGS A-SUBST)))
                       (CDR VAR-SUBST)))
         (or (eq var 'uranus-user::*) (setq @bind nil))))
|#

;;;
;;; Executable pattern:  [pred]
;;;


(DEFUN !@REFUTE (TD TSUBST OPPORNENT OSUBST)
  (cond ((> (td-level td) 0)			;When objectized,
	 (unify-t-n td tsubst oppornent osubst))
	((assigned td tsubst)			;When TD already has value,
         (unify oppornent osubst (cadr @fetched-value) (cddr @fetched-value)))
	(t (let* ((term (td-term td))
		  (pred (td-pred td))
		  (@uranus-world (if (and (not (atom pred))
					  (not (atom (cdr pred)))
					  (eq (second pred) 'uranus-user::||))
;				     (prog1 (cons (fetchvalue (first pred) tsubst)
;						  (cdr tsubst))
;					    (setq pred (cddr pred)))
				     (prog1 (fetchvalue (first pred) tsubst)
					    (setq pred (cddr pred)))
				     (cdr tsubst))))
	     (cond
               ((eq term 'uranus-user::*)
		(cond((lambda-bind 'uranus-user::* tsubst oppornent osubst
				   (refute pred tsubst (1+ @level)))
		      (link td tsubst oppornent osubst))
		     (t (link td tsubst ''@fail@ tsubst) nil)))
	       ((and(unify term tsubst oppornent osubst)
		    (refute pred tsubst (1+ @level)))
		(link td tsubst oppornent osubst))
	       (t (undo @undo-point) (link td tsubst ''@fail@ tsubst) nil))))))


;;; routine to get the definition of a predicate
;;; Definition retrieval

(DEFUN R@GETDEF ($FORM world)
   (AND (LISTP $FORM) (R@GET-PRED-VALUE (CAR $FORM) world)))

(DEFUN R@GET-PRED-VALUE (PRED IND)
   (COND ((SYMBOLP PRED)
	  (r@get-pred-value-from-world-list pred ind))
         ((LISTP PRED) nil)			; lambda form, clause form, lisp form etc.
	 ((td-p pred)
	  (let ((newsubst (newsubst)))	;use newsubst instead of @new-subst(Tomura).
	    (!@REFUTE PRED @OLD-SUBST 'uranus-user::* newsubst)
	    (r@GET-PRED-VALUE (FETCHVALUE 'uranus-user::* newsubst) IND)))
	 (T (REPORT-ERROR "ILLEGAL PREDICATE" PRED) NIL)))


;;; Predicate definitions is a list (NEGATION . ASSERTION)
;;; attached to predicate's PLIST under the flag of WORLDs.

(defun r@get-pred-value-from-world-list (pred ind)
  (cond ((null ind) nil)
	((atom ind) (setq @unseen-world nil) (get pred ind))
	(t (do ((code (get pred (pop ind)) (get pred (pop ind))))
	       ((or code (null ind)) (setq @unseen-world ind)
		(or code (get pred nil)))))))	;NIL is for the system pred, such as MEMBER
    

(DEFUN CALL-LISP-FUNCTION ($NAME $ARGS $SUBST DO-NOT-NEED-VALUE? MACRO?)
   (LET ((ARGS (S@CONVERT $ARGS $SUBST (NOT DO-NOT-NEED-VALUE?))))
    (COND (MACRO? (COND (DO-NOT-NEED-VALUE? (EVAL (CONS $NAME ARGS)))
                        (T (UNIFY (EVAL (CONS $NAME ARGS))
                                  (newsubst '||)
                                  (COND ($ARGS (CAR (LAST $ARGS))) (T NIL))
                                  $SUBST))))
          (DO-NOT-NEED-VALUE? (APPLY $NAME ARGS))
          (t(UNIFY (APPLY $NAME ARGS)
		   (newsubst '||)
		   (s@last $args $subst)
		   @fetched-value)))))
;should never come here
; (@debug1 (report-error "UNDEFINED PREDICATE" (cons $name $args))))))

(defun s@last (x $subst)
  (setq @fetched-value $subst)
  (cond ((null x) nil)
	((varp x)
	 (cond ((assigned x $subst)
		(s@last (cadr @fetched-value)
			(cddr @fetched-value)))
	       (t (let ((v (gensym "*")))
		    (unify x $subst (ncons v) $subst)
		    v))))
	((atom x) nil)
	((cdr x) (s@last (cdr x) $subst))
	(t (car x))))
		  
(DEFUN S@CONVERT (X $SUBST ELIMINATE-LAST?)
   (COND ((VARP X)
          (COND ((ASSIGNED X $SUBST)
                 (S@CONVERT
                    (CADR @FETCHED-VALUE)
                    (CDDR @FETCHED-VALUE)
                    ELIMINATE-LAST?))
                (T x)))				;or (REPORT-ERROR "UNDEFINED VARIABLE" X))))
         ((td-p x)
	  (if (> (td-level x) 0)
	      x
	      (LET ((newsubst (newsubst)))
		(!@refute x $subst '* newsubst)
		(s@convert '* newsubst nil))))
	 ((ATOM X) X)
         ((AND ELIMINATE-LAST? (NULL (CDR X))) NIL)
         ((and (eq (car x) 'quote) (cdr x) (null (cddr x))) ; 'X
          x)
         (T (CONS (S@CONVERT (CAR X) $SUBST NIL)
                  (S@CONVERT (CDR X) $SUBST ELIMINATE-LAST?)))))


;(export '(uranus	; uranus top level function
;	  $FAIL$
;	  retry))

