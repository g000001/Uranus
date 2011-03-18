;;; -*- Mode:Lisp; Package:Amuse; Syntax: Common-lisp; Base: 10; -*-
;;; AMUSE : A Multi Use Structure Editor
;;; (c) H. Nakashima, T. Chikayama and S. Tomura

;;;  Amuse system in Zetalisp using package facility.
;;;  Only the entry function "amuse" is externed into package user.
;;;  Other primitives are in package amuse.
;;;  The alias of package Amuse are : "e", "ec".

;;; Package Declaration is in "uranus:system; system.lisp".

(in-package :amuse)

(export '(getfile putfile fetchvalue) 'amuse)

(shadow '(list-length))

(defvar edit-name nil "")
(defvar editor-get-definition nil "")
(defvar editor-restore-definition nil "")
(defvar editor-execute nil "")

(export '(edit-name editor-get-definition editor-restore-definition editor-execute))

(defvar edit-command nil "")
(defvar entrance-command nil "")
(defvar editor-chain nil "")
(defvar editor-stack nil "")
(defvar editor-variable-prefix #\& "")
(defvar *find* nil "")
(defvar edit-file nil "")

(defvar edit-print? nil "")
(defvar flag nil "")
(defvar prmpt nil "")
(defvar from nil "")
(defvar to nil "")
(defvar repmax  nil "")
(defvar repc nil "")
(defvar *undolist* nil "")
(defvar *item* nil "")
(defvar editor-type nil "")
(defvar printlevel 8. "")

(defvar with-window nil "switch")

(defvar amuse-view-window nil "")		;these windows are created
(defvar amuse-print-window nil "")		;at the first
(defvar  amuse-command-window nil "")		;invokation of AMUSE


(defmacro Uranus-User:edit (name . com)
  `(Edit-lisp ,name ,com))

(DEFUN E@FETCH (X SUBST &OPTIONAL (VALUE (ASSOC X (CDR SUBST))))
   (COND (VALUE (CDR VALUE)) (T X)))

(DEFUN FETCHVALUE (X &OPTIONAL (SUBST '(NIL)) (LEVEL 0.))
   (COND ((E@VARP X)
          (COND ((E@ASSIGNED X SUBST)
                 (FETCHVALUE (E@FETCH X SUBST) '(NIL) LEVEL))
                (T X)))
         ((ATOM X) X)
         ((ZEROP (1- LEVEL)) 'Uranus-User:?)
         (T (CONS (FETCHVALUE (CAR X) SUBST (1- LEVEL))
                  (FETCHVALUE (CDR X) SUBST LEVEL)))))

;;; The entry function of Amuse system.
(DEFUN Uranus-User:AMUSE (EDIT-NAME &OPTIONAL (ENTRANCE-COMMAND NIL) (with-window nil))
  (if with-window (set-up-amuse-windows))
  (CATCH ':EDIT
    (LET ((EDITOR-CHAIN NIL) (EDITOR-STACK NIL) (*FIND* NIL) 
	  (PRINTLEVEL 4.) (EDITOR-VARIABLE-PREFIX #\&)
	  (*standard-input* (if with-window amuse-command-window *terminal-io*))
	  (*standard-output* (if with-window amuse-print-window *terminal-io*)))
      (E@GETDEF EDIT-NAME)
      (CATCH ':EDITLOOP
	(DO ((EDIT-PRINT? NIL))
	    (NIL)
	  (COND (ENTRANCE-COMMAND
		 (E@INTERPRET (POP ENTRANCE-COMMAND)))
		(T (RETURN NIL)))))
      (ec@v) (ec@p)
      (LET ((EDIT-PRINT? T)) (E@LOOP))))
  #+LISPM (if with-window (zl:send *terminal-io* ':expose))
  #+LISPM (if with-window (zl:send *terminal-io* ':select)))

#-symbolics
(defun set-up-amuse-windows () nil)

#+symbolics
(defun set-up-amuse-windows ()
  (multiple-value-bind (x0 y0 x y)
      (zl:send *terminal-io* :edges)
    (let ((xmin (/ (+ x0 x) 2.))
	  (ystep (/ (- y 100) 2.)))
      (or amuse-view-window
	  (setq amuse-view-window
		(tv:make-window 'tv:window
				':name "AMUSE Current Position"
				':blinker-p nil	; nil may raise an error in old system.
				':edges-from `(,xmin ,y0 ,x ,(+ y0 ystep)))))
      
      (or amuse-print-window
	  (setq amuse-print-window
		(tv:make-window 'tv:window
				':name "AMUSE Current Scope"
				':blinker-p nil
				':edges-from `(,xmin ,(+ y0 ystep) ,x ,(- y 100)))))
      
      (or amuse-command-window
	  (setq amuse-command-window
		(tv:make-window 'tv:window
				':name "AMUSE Command"
				':edges-from `(,xmin ,(- y 100) ,x ,y)))))
      
    (zl:send amuse-view-window ':expose)
    (zl:send amuse-print-window ':expose)
    (zl:send amuse-command-window ':expose)
    (zl:send amuse-command-window ':clear-window)
    (zl:send amuse-command-window ':select)))

(DEFUN Edit-lisp (EDIT-NAME COM)
  (COND ((or(SYMBOLP EDIT-NAME)(listp edit-name))
	    (LET ((EDITOR-TYPE edit-name) 
		  (EDITOR-GET-DEFINITION
		    #'(LAMBDA (X)
			(FETCHVALUE (symbol-function x))))
		  (EDITOR-RESTORE-DEFINITION
		    #'(LAMBDA (DEF)
			(setf (symbol-function edit-name) (fetchvalue def))))
		  (EDITOR-EXECUTE #'(LAMBDA (x) (PRINT (EVAL x)))))
	      (URANUS-USER:AMUSE EDIT-NAME COM)))
	(T (LET ((EDITOR-TYPE NIL) 
		 (EDITOR-GET-DEFINITION #'(LAMBDA (X) (GETFILE X))) 
		 (EDITOR-RESTORE-DEFINITION
		   #'(LAMBDA (DEF) (PUTFILE EDIT-NAME DEF)))
		 (EDITOR-EXECUTE #'(LAMBDA (x) (PRINT (EVAL x)))))
             (URANUS-USER:AMUSE EDIT-NAME COM)))))

(DEFUN E@LOOP NIL
   (DO () (NIL)
     (CATCH ':EDITLOOP
       #+LISPM (if with-window (zl:send *standard-input* ':select))
       (E@INTERPRET (read *standard-input*)))))

(DEFUN E@INTERPRET (EDIT-COMMAND)
   (COND ((NUMBERP EDIT-COMMAND) (E@MOVE EDIT-COMMAND) (ec@v) (ec@p))
         ((SYMBOLP EDIT-COMMAND) (e@interpret-symbol))
         (T (funcall editor-execute edit-command))))

(defun ec@? ()
       (terpri *standard-input*)
       (PRINC "editing " *standard-input*)
       (PRIN1 EDIT-NAME *standard-input*))

(defun ec@n () (e@next))

(defun ec@b () (e@before))

(defun ec@p ()
       #+LISPM (if with-window (zl:send amuse-print-window ':clear-window))
       #+LISPM (if with-window (zl:send amuse-print-window ':select))
       (Pprint (FETCHVALUE (CAAR EDITOR-CHAIN) '(NIL) PRINTLEVEL)))

(defun ec@pp () (PPRINT (CAAR EDITOR-CHAIN)))

(defun ec@in0 () (Ec@in (E@READ)))

(defun ec@i (x) (Ec@in x))

(defun ec@ib0 () (Ec@ib (E@READ)))

(defun ec@it0 () (E@MOVE 1.) (Ec@ib (E@READ)) (E@MOVE 0.))

(defun ec@k () (E@DELETE))

(defun ec@d () (E@DELETE) (POP EDITOR-STACK))

(defun ec@e0 () (ec@e (e@read)))

(defun ec@e (x) (E@GETDEF x))

(defun ec@c () (PUSH (FETCHVALUE (CAAR EDITOR-CHAIN)) EDITOR-STACK))

(defun ec@f0 () (Ec@f (E@READ)))

(defun ec@v () (E@VIEW))

(defun ec@fn () (E@FINDNEXT *FIND*))

(defun ec@r0 () (Ec@r (E@READ)))

(defun ec@ra0 () (Ec@ra (E@READ) (E@READ)))

(defun ec@r1 () (Ec@ra (E@READ) (E@READ) 1.))

(defun ec@r2 () (Ec@ra (E@READ) (E@READ) 2.))

(defun ec@r3 () (Ec@ra (E@READ) (E@READ) 3.))

(defun ec@var0 () (ec@var (e@read)))

#+maclisp
(defun ec@var (x) (SETQ EDITOR-VARIABLE-PREFIX (car (exploden x))))
#+symbolics
(defun ec@var (x) (setf editor-variable-prefix (aref (symbol-name x) 0)))

(defun ec@z () (THROW ':EDIT NIL))

(defun ec@s () (E@RESTORE (CAAR (LAST EDITOR-CHAIN))))

(defun ec@sc0 () (ec@sc (e@read)))

(defun ec@sc (edit-name)
  (E@RESTORE (CAAR (LAST EDITOR-CHAIN))))

(defun ec@q () (THROW ':EDIT (E@RESTORE (CAAR (LAST EDITOR-CHAIN)))))


(defun ec@st () (PPRINT (FETCHVALUE EDITOR-STACK '(NIL) PRINTLEVEL)))

(defun ec@stack () (PPRINT EDITOR-STACK))

(defun ec@top () (SETQ EDITOR-CHAIN (LAST EDITOR-CHAIN)))

(defun ec@level0 () (ec@level (read)))

(defun ec@level (n) (SETQ PRINTLEVEL n))

(defun ec@l () (ec@last))

(defun ec@bi0 () (ec@bi (E@RANGE (READ)) (E@RANGE (READ))))

(defun ec@u0 () (ec@u (e@read)))

(DEFUN E@GETDEF (NAM1)
  (DO ((DEF NIL)
       (*standard-output* (if with-window amuse-command-window *terminal-io*)))
      (NIL)
    (COND ((NULL NAM1)
	   (if with-window (format t "Editing ~A continues~%" edit-name))
	   (RETURN NIL))
	  ((SETQ DEF (FUNCALL EDITOR-GET-DEFINITION NAM1))
	   (SETQ EDIT-NAME NAM1)
	   (RETURN (SETQ EDITOR-CHAIN (cons (CONS DEF nil) nil))))
	  (T (PRINT NAM1)
	     (PRINC " is not defined. Enter correct name or NIL: ")
	     (SETQ NAM1 (read))))))

(DEFUN E@READ NIL
   (LET ((COM (COND ((not (atom ENTRANCE-COMMAND)) (POP ENTRANCE-COMMAND))
                    (T (READ)))))
    (COND ((NUMBERP COM)
           (COND ((< COM 0.) (E@ERR "Illegal stack position"))
                 ((= COM 0.) (FETCHVALUE (CAAR EDITOR-CHAIN)))
                 ((>= (LIST-LENGTH EDITOR-STACK) COM)
                  (FETCHVALUE (NTH (1- COM) EDITOR-STACK)))
                 (T (E@ERR "Stack too short"))))
          ((AND (LISTP COM)
                (EQ (CAR COM) 'QUOTE)
                (CDR COM)
                (NUMBERP (SECOND COM)))
           (SECOND COM))
          (T COM))))

(DEFUN E@ERR (MES)
       (let ((*standard-output* (if with-window amuse-command-window *terminal-io*)))
	 (PRINC " : ") (PRINC MES) (terpri) (THROW ':EDITLOOP NIL)))

(DEFUN E@NEXT NIL
   (LET ((HC (POP EDITOR-CHAIN)))
    (COND ((NULL EDITOR-CHAIN) (PUSH HC EDITOR-CHAIN) (E@ERR "Top level"))
          ((NULL (CDDR HC)) (PUSH HC EDITOR-CHAIN) (E@ERR "Last element"))
          (T (PUSH (CONS (SECOND (CDR HC)) (CDR (CDR HC))) EDITOR-CHAIN)))))

(DEFUN E@BEFORE NIL
       (LET ((HC (POP EDITOR-CHAIN)))
            (cond ((NULL EDITOR-CHAIN)
                   (PUSH HC EDITOR-CHAIN) (E@ERR "Top level"))
                  ((eq (caar editor-chain) (cdr hc))
                   (push hc editor-chain) (e@err "First element"))
                  (t (DO ((W (CAAR EDITOR-CHAIN) (CDR W)))
                         ((EQ (CDR W) (CDR HC))
                          (PUSH (CONS (CAR W) W) EDITOR-CHAIN)))))))

(DEFUN Ec@in (ITEM)
   (LET ((HC (POP EDITOR-CHAIN)))
    (COND (EDITOR-CHAIN
             (RPLACD (CDR HC) (CONS ITEM (CDDR HC)))
             (PUSH (CONS ITEM (CDDR HC)) EDITOR-CHAIN))
          (T (PUSH HC EDITOR-CHAIN)
             (PUSH ITEM EDITOR-STACK)
             (E@ERR "Top level")))))

(DEFUN E@DELETE NIL
   (LET ((HC (POP EDITOR-CHAIN)))
    (COND (EDITOR-CHAIN
             (PUSH (POP HC) EDITOR-STACK)
             (COND ((CDR HC)
                    (RPLACA HC (SECOND HC))
                    (RPLACD HC (CDDR HC))
                    (PUSH (CONS (CAR HC) HC) EDITOR-CHAIN))
                   ((EQ HC (CAAR EDITOR-CHAIN))
                    ; one element list
                    (RPLACA EDITOR-CHAIN
                       (CONS NIL
                             (COND ((CDAR EDITOR-CHAIN)
                                    (RPLACA (CDAR EDITOR-CHAIN) NIL))
                                   (T NIL)))))
                   (T (DO ((W (CAAR EDITOR-CHAIN) (CDR W)))
                          ((EQ (CDR W) HC) (RPLACD W (CDDR W)))))))
          (T (PUSH HC EDITOR-CHAIN) (E@ERR "Top level")))))

(DEFUN E@MOVE (N)
   (LET ((HEAD (CAAR EDITOR-CHAIN)))
    (COND ((ZEROP N)
           (COND ((CDR EDITOR-CHAIN) (POP EDITOR-CHAIN))
                 (T (E@ERR "Top level"))))
          ((LISTP HEAD)
           (SETQ N (E@RANGE N))
           (PUSH (CONS (NTH (1- N) HEAD) (NTHCDR (1- N) HEAD))
                 EDITOR-CHAIN))
          (T (E@ERR "Atomic scope")))))

(DEFUN Ec@ib (ITEM)
   (LET ((HC (POP EDITOR-CHAIN)))
    (COND (EDITOR-CHAIN
             (RPLACD (CDR HC) (CONS (CADR HC) (CDDR HC)))
             (RPLACA (CDR HC) ITEM)
             (PUSH (CONS (CAR HC) (CDDR HC)) EDITOR-CHAIN))
          (T (PUSH HC EDITOR-CHAIN)
             (PUSH ITEM EDITOR-STACK)
             (E@ERR 'TOPLEVEL)))))

(DEFUN E@RESTORE (DEFS)
   (FUNCALL EDITOR-RESTORE-DEFINITION DEFS)
   (AND EDIT-PRINT? (PRINT EDIT-NAME *standard-input*)
	(PRINC " restored" *standard-input*))
   T)

(DEFUN Ec@r (ITEM)
   (LET ((HC (POP EDITOR-CHAIN)))
    (COND (EDITOR-CHAIN
             (POP HC)
             (RPLACA HC ITEM)
             (PUSH (CONS ITEM HC) EDITOR-CHAIN))
          (T (PUSH (CAR HC) EDITOR-STACK)
             (SETQ EDITOR-CHAIN (CONS (CONS ITEM nil) nil))))))

(DEFUN Ec@ra (FROM TO &OPTIONAL (REPMAX 100.) (REPC 0.) (*UNDOLIST* NIL) 
                     (S (CONS NIL nil)))
   (COND ((E@UNIFY FROM S (CAAR EDITOR-CHAIN))
          (COND ((CDR EDITOR-CHAIN)
                 (RPLACA (CDAR EDITOR-CHAIN)
                    (CAR (RPLACA (CAR EDITOR-CHAIN) (FETCHVALUE TO S)))))
                (T (RPLACA (CAR EDITOR-CHAIN) (FETCHVALUE TO S))))
          (REP-COUNT))
         (T (CATCH 'REPLACEALL (E@REPALL (CAAR EDITOR-CHAIN)))))
   (AND EDIT-PRINT? (PRINT REPC *standard-input*)
	(PRINC " replaced. " *standard-input*)))

(DEFUN E@REPALL (L)
  (let((S (CONS NIL nil)))
    (COND ((ATOM L))
          (T (COND ((E@UNIFY FROM S (CAR L))
                    (RPLACA L (FETCHVALUE TO S))
                    (REP-COUNT))
                   (T (E@REPALL (CAR L))))
             (COND ((AND (ATOM (CDR L))
                         (E@UNIFY FROM (SETQ S (CONS NIL nil)) (CDR L)))
                    (RPLACD L (FETCHVALUE TO S))
                    (REP-COUNT))
                   (T (E@REPALL (CDR L))))))))

(DEFUN REP-COUNT NIL
   (COND ((EQ (SETQ REPC (1+ REPC)) REPMAX) (THROW 'REPLACEALL NIL))))

(DEFUN Ec@pop NIL
   (COND ((NULL EDITOR-STACK) (E@ERR "stack empty"))
         (T (POP EDITOR-STACK))))

(defun ec@o () (Ec@pop))

(DEFUN Ec@f (*ITEM*)
  (setq *find* *item*)
  (LET ((*UNDOLIST* NIL)) (OR (E@FIND1) (E@ERR "not found"))))

(DEFUN E@FIND1 NIL
   (LET ((X (CAAR EDITOR-CHAIN)))
    (COND ((E@UNIFY *ITEM* (CONS NIL nil) X))
          ((ATOM X) NIL)
          (T (DO ((LENGTH (LIST-LENGTH X)) (N 1. (1+ N)))
                 ((> N LENGTH))
                 (E@MOVE N)
                 (COND ((E@FIND1) (RETURN T)) (T (POP EDITOR-CHAIN))))))))

;;; *** "length" belongs to "global". So below def. will destroy the system function.
;;;  To avoid this, .....
#|(defun list-length (x)
  (do ((i 0 (1+ i))
       (y x (cdr y)))
      ((atom y) i)))|#

(DEFUN E@FINDNEXT (*ITEM*)
   (LET ((OLDCHAIN EDITOR-CHAIN))
    (DO NIL
        (NIL)
        (COND ((NULL (CDR EDITOR-CHAIN))
               (SETQ EDITOR-CHAIN OLDCHAIN)
               (E@ERR "not found"))
              (T (LET ((X (cdr(POP EDITOR-CHAIN))))
                   (COND ((ATOM (CDR X))) ; last element
                         (T (PUSH (CONS (SECOND X) (CDR X)) EDITOR-CHAIN)
                            (AND (E@FIND1) (RETURN T))))))))))

(DEFUN Ec@last NIL (E@MOVE (LIST-LENGTH (CAAR EDITOR-CHAIN))))

(DEFUN Ec@bi (from to)
   (LET ((HEAD (CAAR EDITOR-CHAIN)))
    (COND ((ATOM HEAD) (E@ERR "atomic scope"))
          (T (LET ((W (NTHCDR (1- from) HEAD)) (WW (NTHCDR (1- to) HEAD)))
               (COND ((> from to)
                      (E@ERR "The first operand exceeds the second"))
                     (T (RPLACD (RPLACA W (CONS (CAR W) (CDR W))) (CDR WW))
                        (RPLACD (COND ((EQ W WW) (CAR WW)) (T WW)) NIL))))))))

(DEFUN Ec@ri NIL
   (LET ((HC (POP EDITOR-CHAIN)))
    (LET ((UP (COND (EDITOR-CHAIN (CAAR EDITOR-CHAIN))
                    (T (PUSH HC EDITOR-CHAIN) (E@ERR "Top level"))))
          (W (CDR HC)))
     (RPLACD (RPLACA UP (CONS (CAR UP) (CDR UP))) (CDR W))
     (RPLACD (COND ((EQ W UP) (CAR W)) (T W)) NIL)
     (PUSH (CONS (CAR UP) UP) EDITOR-CHAIN)
     (PUSH (CONS (CAR (COND ((EQ W UP) (CAR W)) (T W)))
                 (COND ((EQ W UP) (CAR W)) (T W)))
           EDITOR-CHAIN))))

(DEFUN Ec@li NIL
   (LET ((HC (POP EDITOR-CHAIN)))
    (LET ((W (COND (EDITOR-CHAIN (CDR HC))
                   (T (PUSH HC EDITOR-CHAIN) (E@ERR "Top level")))))
     (RPLACD (RPLACA W (CONS (CAR W) (CDR W))) NIL)
     (PUSH (CONS (CAR W) (CDR HC)) EDITOR-CHAIN)
     (PUSH (CONS (CAR HC) (CAR W)) EDITOR-CHAIN))))

(DEFUN Ec@bo NIL
   (LET ((HC (COND ((ATOM (CAAR EDITOR-CHAIN)) (E@ERR "Atomic item"))
                   (T (POP EDITOR-CHAIN)))))
    (LET ((W (CDR HC)))
     (OR EDITOR-CHAIN (PROG2 (PUSH HC EDITOR-CHAIN) (E@ERR "Top level")))
     (RPLACA (RPLACD W (NCONC (CDAR W) (CDR W))) (CAAR W)))))

(DEFUN E@RANGE (N)
   (LET ((HEAD (CAAR EDITOR-CHAIN)))
    (OR (NUMBERP N) (E@ERR "number expected"))
    (AND (MINUSP N) (SETQ N (+ (LIST-LENGTH HEAD) N 1.)))
    (OR (AND (<= 1. N) (<= N (LIST-LENGTH HEAD))) (E@ERR "List too short")))
   N)

(DEFUN PUTFILE (FILE X)
   (DO ((*STANDARD-OUTPUT* (OPEN FILE :direction :output)))
       ((NULL X) (CLOSE *STANDARD-OUTPUT*))
       (PPRINT (POP X))))

(DEFUN GETFILE (EDIT-FILE)
   (CATCH 'GETFILE
           (LET ((*STANDARD-INPUT* (OPEN EDIT-FILE :direction :input)) (EDIT-LOAD NIL))
            (DO ((ITEM (READ *STANDARD-INPUT* 'EOF) (READ *STANDARD-INPUT* 'EOF)))
                ((EQ ITEM 'EOF)
                 (CLOSE *STANDARD-INPUT*)
                 (THROW 'GETFILE (NREVERSE EDIT-LOAD)))
                (PUSH ITEM EDIT-LOAD)))))

(defun e@view nil
       #+LISPM (if with-window (zl:send amuse-view-window ':clear-window))
       #+LISPM (if with-window (zl:send amuse-view-window ':select))
       (LET ((HC (CAAR EDITOR-CHAIN))
	     (*standard-output* (if with-window amuse-view-window *terminal-io*)))
	 (COND ((CDR EDITOR-CHAIN)
		(pprint
		  (FETCHVALUE
		    (MAPCAR (FUNCTION
			      (LAMBDA (X) (COND ((EQ X HC) 'URANUS-USER:$$$) (T X))))
			    (CAR (SECOND EDITOR-CHAIN)))
		    '(NIL)
		    3.)
		  nil))
	       (T (princ "Top level" *standard-output*)))))

(defun e@stack () editor-stack)

(defun e@scope () (caar editor-chain))

(DEFUN E@UNIFY (X SUBST Y)
   (COND ((E@VARP X)
          (COND ((E@ASSIGNED X SUBST) (EQUAL (E@FETCH X SUBST) Y))
                (T (E@LINK X SUBST Y))))
         ((ATOM Y) (EQUAL X Y))
         ((ATOM X) NIL)
         ((E@UNIFY (CAR X) SUBST (CAR Y)) (E@UNIFY (CDR X) SUBST (CDR Y)))))

(defun e@varp (x)
  (and (symbolp x) (eq (aref (symbol-name x) 0) editor-variable-prefix)))

(DEFUN E@ASSIGNED (X SUBST) (ASSOC X (CDR SUBST)))

(DEFUN E@LINK (X XSUBST Y) (RPLACD XSUBST (CONS (CONS X Y) (CDR XSUBST))))

(DEFUN Ec@u (UPTO)
   (DO ((HC (CDR EDITOR-CHAIN) (CDR HC)))
       ((NULL HC) (E@ERR "No such upper structure found"))
       (COND ((AND (LISTP (CAAR HC)) (EQUAL (CAAAR HC) UPTO))
              (RETURN (SETQ EDITOR-CHAIN HC))))))

(DEFmacro EXECM (C)
   `(CATCH ':EDITLOOP
           (COND ((BOUNDP 'EDITOR-CHAIN)
                  (DO ((ENTRANCE-COMMAND ,C))
                      ((NULL ENTRANCE-COMMAND) T)
                      (E@INTERPRET (POP ENTRANCE-COMMAND))))
                 (T (PRINC "EXECM is called outside the editor") NIL))))


(defun e@interpret-symbol ()
 (CASE EDIT-COMMAND
             (URANUS-USER:N (Ec@N) (ec@p) (ec@v))
             (URANUS-USER:B (Ec@B) (ec@p) (ec@v))
             (URANUS-USER:P (ec@p))
             (URANUS-USER:PP (ec@pp))
             ((URANUS-USER:I URANUS-USER:IN) (ec@in0) (ec@v) (ec@p))
             (URANUS-USER:IB (ec@ib0) (ec@v) (ec@p))
             (URANUS-USER:IT (ec@it0) (ec@p))
             (URANUS-USER:K (ec@k) (ec@v) (ec@p))
             (URANUS-USER:D (ec@d) (ec@v) (ec@p))
             (URANUS-USER:E (ec@e0) (ec@v) (ec@p))
             (URANUS-USER:C (ec@c) (ec@p))
             (URANUS-USER:F (ec@f0) (ec@v) (ec@p))
             (URANUS-USER:V (ec@v))
             (URANUS-USER:FN (ec@fn) (ec@v) (ec@p))
             (URANUS-USER:R (ec@r0) (ec@p))
             (URANUS-USER:RA (ec@ra0) (ec@p))
             (URANUS-USER:R1 (ec@r1) (ec@p))
             (URANUS-USER:R2 (Ec@r2) (ec@p))
             (URANUS-USER:R3 (Ec@r3) (ec@p))
             (URANUS-USER:VAR (ec@var0))
             (URANUS-USER:Z (ec@z))
             (URANUS-USER:S (ec@s))
             (URANUS-USER:SC (ec@sc0))
             (URANUS-USER:Q (ec@q))
             ((URANUS-USER:O URANUS-USER:POP) (ec@pop))
             (URANUS-USER:ST (ec@st))
             (URANUS-USER:STACK (ec@stack))
             (URANUS-USER:TOP (ec@top) (ec@v) (ec@p))
             (URANUS-USER:LEVEL (ec@level0))
             (URANUS-USER:X (FUNCALL EDITOR-EXECUTE (e@read)))
             ((URANUS-USER:LAST URANUS-USER:L) (Ec@last) (ec@v) (ec@p))
             (URANUS-USER:LI (Ec@li) (ec@v) (ec@p))
             (URANUS-USER:RI (Ec@ri) (ec@v) (ec@p))
             (URANUS-USER:BI (Ec@bi0) (ec@v) (ec@p))
             (URANUS-USER:BO (Ec@bo) (ec@v) (ec@p))
             (URANUS-USER:U (ec@u0) (ec@v) (ec@p))
             (Uranus-User:? (ec@?))
             (T (E@ERR "Undefined command"))))

;; Interface to Uranus

(MAPC (FUNCTION (LAMBDA (X) (setf (get X :LISP-PREDICATE) T)))
      '(ec@in ec@ib ec@it ec@e ec@f ec@r ec@ra ec@var ec@sc ec@level ec@bi ec@u))


