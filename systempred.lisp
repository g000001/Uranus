;;; -*- Mode:Lisp; Base:10.; Package: URA; Syntax: Common-lisp -*-
;;; This file is "Systempred.lisp".
;;; Uranus Interpreter
;;; for Zetalisp
;;; 1984/03/24
;;; by H. Nakashima and S. Tomura

(in-package :uranus) 
#+kcl(make-package 'uranus-code :use nil)
#+lucid (proclaim '(optimize speed (safety 0)))

(shadow 'assert)

(defvar unseen-defs nil "")
(defvar unseen-world nil "")
(defvar retry nil "")
(defvar vars nil "")
(DEFVAR @TRACELEVEL)
;(defvar @saved-vars nil "")

;;; def-sys-pred should be a macro so that Uranus code is compiled.
		 
;#+3600
;(defmacro def-sys-pred* (name args .  body)
;  (let ((u-name (intern (symbol-name name) 'user)))
;    `(progn 
;       'cl-user::compile
;       (defun (,name cl-user::uranus-system-code) ()
;	 ,(cond ((cdr body) `(and . ,body))
;		(t (car body))))
;       (eval-when (eval load)
;	 (putprop ',u-name (cons ',args 
;				 #'(:property ,name cl-user::uranus-system-code)) :code)))))

(defmacro def-sys-pred* (name args .  body)
  (let ((u-name (intern (symbol-name name) :uranus-user))
	#+kcl(c-name (intern (symbol-name name) :uranus-code))
	)
    `(progn
       #+symbolics
       (defun (,name cl-user::uranus-system-code) ()
	 ,(cond ((cdr body) `(and . ,body))
		(t (car body))))

       #+kcl
       (progn
	 (defun ,c-name ()
	 ,(cond ((cdr body) `(and . ,body))
		(t (car body))))
	 (setf (get ',name 'cl-user::uranus-system-code)
	       #',c-name))

       #+(and (not symbolics) (not kcl))
       (setf (get ',name 'uranus-user::uranus-system-code)
	      #'(lambda ()
		  ,(cond ((cdr body) `(and . ,body))
			 (t (car body)))))

       (setf (get ',u-name ':code)
	     (cons ',args
		   (get ',name 'uranus-user::uranus-system-code))))))


;#+3600
;(defmacro def-sys-pred (names args . body)
;  (cond ((symbolp names) `(def-sys-pred* ,names ,args . ,body))
;	(t
;	 `(progn 
;	    'uranus-user::compile
;	    . , (loop for x in names collect `(def-sys-pred* ,x ,args . ,body))))))

(defmacro def-sys-pred (names args . body)
  (cond ((symbolp names) `(def-sys-pred* ,names ,args . ,body))
	(t
	 `(eval-when (compile eval load)
		     . ,(mapcar  #'(lambda (x) `(def-sys-pred ,x ,args . ,body))
				 names)))))

;#+3600
;(defmacro def-sys-multi-pred* (name args body vars retry)
;  (let ((u-name (intern (symbol-name name) 'user)))
;    `(progn 
;       'uranus-user::compile
;       (defun (,name uranus-user::uranus-system-code) ()
;	 (let ((@undo-point @undolist))
;	   (if ,body
;	       (r@push-alternatives '(retry ,retry) nil nil @subst
;				    (cons (append '(vars retry) ',vars)
;					  (append '(,vars ,retry) (evlis ',vars)))
;				    ))))
;       (eval-when (eval load)
;	 (putprop ',u-name (cons ',args 
;				 #'(:property ,name uranus-user::uranus-system-code)) :code)))))

(defmacro def-sys-multi-pred* (name args body vars retry)
  (let ((u-name (intern (symbol-name name) :uranus-user))
	#+kcl (c-name (intern (symbol-name name) :uranus-code)))
    `(progn
       #+symbolics
       (defun (,name uranus-user::uranus-system-code) ()
	 (let ((@undo-point @undolist))
	   (if ,body
	       (r@push-alternatives '(retry ,retry) nil nil @subst
				    (cons (append '(vars retry) ',vars)
					  (append '(,vars ,retry) (evlis ',vars)))
				    ))))
       #+kcl
       (progn
	 (defun ,c-name ()
	   (let ((@undo-point @undolist))
	     (if ,body
		 (r@push-alternatives '(retry ,retry) nil nil @subst
				      (cons (append '(vars retry) ',vars)
					    (append '(,vars ,retry) (evlis ',vars)))
				      ))))
	 (setf (get ',name 'uranus-user::uranus-system-code)
	       #',c-name))

       #+(and (not symbolics) (not kcl))
       (setf (get ',name 'uranus-user::uranus-system-code)
		      #'(lambda ()
			  (let ((@undo-point @undolist))
			    (if ,body
				(r@push-alternatives '(retry ,retry) nil nil @subst
						     (cons (append '(vars retry) ',vars)
							   (append '(,vars ,retry) (evlis ',vars)))
						     )))))
       (setf (get ',u-name ':code)
	     (cons ',args 
		   (get ',name 'uranus-user::uranus-system-code))))))

;;; Def-sys-internal defines system predicates which are used intenally
;;; such as RETRY.  They should not appear on STEP as it is.

(defmacro def-sys-internal (name args .  body)
  (let ((u-name (intern (symbol-name name) :uranus))
	#+kcl (c-name (intern (symbol-name name) :uranus-code)))
    `(progn
       #+symbolics
       (defun (,name uranus-user::uranus-system-code) ()
	      ,(cond ((cdr body) `(and . ,body))
		     (t (car body))))
       #+kcl
       (progn
	 (defun ,c-name ()
	   ,(cond ((cdr body) `(and . ,body))
		  (t (car body))))
	 (setf (get ',name 'uranus-user::uranus-system-code)
	       #',c-name))
       #+(and (not symbolics) (not kcl))
       (setf (get ',name 'uranus-user::uranus-system-code)
	     #'(lambda ()
		 ,(cond ((cdr body) `(and . ,body))
			(t (car body)))))
       
       (setf (get ',u-name ':code)
	     (cons ',args
		   (get ',name 'uranus-user::uranus-system-code))))))

(defun evlis (l)
  (do ((x (pop l) (pop l)) (y nil))
      ((null l) (nreverse (push (eval x) y)))
      (push (eval x) y)))

(defmacro with-vars (l1 l2 . body)
 `(eval `(let ,(pair ',(eval l1) ',(eval l2)) . ,',body)))


(defun pair (l1 l2)
  (cond ((null l1) nil)
	(t (cons (list (car l1) `',(car l2))
		 (pair (cdr l1) (cdr l2))))))

(def-sys-internal retry (*form)
  (progv (car @saved-vars) (cdr @saved-vars)
    (if (eval (car @args))
	(r@push-alternatives `(retry ,retry) nil nil @old-subst
			     (cons (append '(vars retry) vars)
				   (append `(,vars ,retry) (evlis vars)))
			     ))))

(defmacro def-sys-multi-pred (names args body vars retry)
  (cond ((symbolp names) `(def-sys-multi-pred* ,names ,args ,body ,vars ,retry))
	(t
	 `(eval-when (compile eval load)
		     . , (mapcar #'(lambda (x)
				     `(def-sys-multi-pred*
					,x ,args ,body ,vars ,retry))
				 names)))))


(DEFUN INIT-SYSTEM-CODE NIL
     (init-lispfunctions))


;;; Macro for setting up the environment for execution
;;; arguments of control-predicates.

(defmacro with-new-env (form clause pred)
          `(let* ((@form ,form)
		  (@clause ,clause)
		  (@unseen-world @uranus-world)
		  (@definitions (r@getdef @form @uranus-world))
		  (@negations (pop @definitions))
		  (@old-subst $subst)
		  (@new-subst (newsubst))
		  (@undo-point @undolist)
		  (@final-undo-point @undolist)
		  (@stack nil)
		  (@cue nil)
		  (@level (1+ @level))
		  (@world-at-entrance @uranus-world))
	     ,pred))

;;; Predicates data-base manupilations

(defun assert-getdef ($form world)
  (cond ((not (listp $form)) nil)
	(@reload-flag
	 (cond ((member (cons (car $form) world) @reload-deflist :test #'equal)
		(r@get-pred-value (car $form) world))
	       (t (push (cons (car $form) world) @reload-deflist) nil)))
	(t (r@get-pred-value (car $form) world))))

(defmacro assert-handler (form)
  `(if (symbolp (caar clause))
       (let ((old-def (assert-getdef (CAR CLAUSE) (car @uranus-world))))
	 (setf (get (CAAR CLAUSE) (CAR @URANUS-WORLD))
	       ,form)
	 (P@REGISTER-INTO-WORLD (CAR @URANUS-WORLD) (CAAR CLAUSE)))
       (report-error "ILLEGAL ARGUMENT TO ASSERT*" clause)))

(DEFUN ASSERT (CLAUSE)
  (assert-handler
    (cons (car old-def)
	  (ASSERT-POS (cdr old-def)
		      (CONS (CDAR CLAUSE) (CDR CLAUSE))))))

(DEFUN ASSERT-POS (OLDDEF NEWDEF)
  (COND ((NULL OLDDEF) (NCONS NEWDEF))
        ((null (car olddef)) (cons newdef olddef))
        ((AND (ASSERT-UNIT NEWDEF) (NOT (ASSERT-UNIT (CAR OLDDEF))))
         (CONS NEWDEF OLDDEF))
        ((AND (ASSERT-UNIT (CAR OLDDEF)) (NOT (ASSERT-UNIT NEWDEF)))
         (CONS (CAR OLDDEF) (ASSERT-POS (CDR OLDDEF) NEWDEF)))
        ((ASSERT-SPECIFICP (CAR NEWDEF) (CAAR OLDDEF)) (CONS NEWDEF OLDDEF))
        (T (CONS (CAR OLDDEF) (ASSERT-POS (CDR OLDDEF) NEWDEF)))))

(DEFUN ASSERT-SPECIFICP (P1 P2)
  (COND ((VARP P1) NIL)
        ((VARP P2) T)
        ((ATOM P1) (NOT (ATOM P2)))
        ((ATOM P2) NIL)
        (T (DO ((X (POP P1) (POP P1)) (Y (POP P2) (POP P2)))
               (NIL)
             (COND ((ASSERT-SPECIFICP X Y) (RETURN T))
                   ((AND (ATOM Y) (OR (LISTP X) (VARP X))) (RETURN NIL))
                   ((VARP P1) (RETURN NIL))
                   ((VARP P2) (RETURN T))
                   ((ATOM P1) (RETURN (NOT (ATOM P2))))
                   ((ATOM P2) (RETURN NIL)))))))

(DEFUN ASSERT-UNIT (DEF) (NULL (CDR DEF)))

(DEFUN ASSERTZ (CLAUSE)
  (assert-handler
    (cons (car old-def)
	  (assert-last (CONS (CDAR CLAUSE) (CDR CLAUSE))
		       (cdr old-def)))))

(defun assert-last (clause defs)
  (cond ((null defs) (ncons clause))
        ((null (car defs)) (cons clause defs))
        (t (cons (car defs) (assert-last clause (cdr defs))))))

(DEFUN ASSERTA (CLAUSE)
  (assert-handler
    (cons (car old-def)
	  (CONS (CONS (CDAR CLAUSE) (CDR CLAUSE))
		(cdr old-def)))))

(defun denya (clause)
  (assert-handler
    (cons (cons (cons (cdar clause) (cdr clause))
		(car old-def))
	  (cdr old-def))))

(defun denyz (clause)
  (assert-handler
    (cons (append (car old-def)
		  (ncons (cons (cdar clause) (cdr clause))))
	  (cdr old-def))))

;;;
;;; Initialize predicates for definitions
;;;

(DEF-SYS-PRED (ASSERT AS) ((URANUS-USER::? . URANUS-USER::?) . URANUS-USER::?)
  (ASSERT (FETCHVALUE @ARGS @old-subst)))

(DEF-SYS-PRED (ASSERTq Aq) ((Uranus-User::? . Uranus-User::?) . Uranus-User::?)	;Should be used at the
  (ASSERTz (fetchvalue-q @args @old-subst))
  ;;;(setq @goal 'uranus-user::ok)
  )

(DEF-SYS-PRED (ASSERTA aa) ((URANUS-USER::? . URANUS-USER::?) . URANUS-USER::?)
  (ASSERTA (FETCHVALUE @ARGS @old-subst)))

(DEF-SYS-PRED (ASSERTZ AZ) ((URANUS-USER::? . URANUS-USER::?) . URANUS-USER::?)
  (ASSERTZ (FETCHVALUE @ARGS @old-subst)))

(def-sys-pred deny ((uranus-user::? . uranus-user::?) . uranus-user::?)
  (denyz (fetchvalue @args @old-subst)))

(def-sys-pred denya ((uranus-user::? . uranus-user::?) . uranus-user::?)
  (denya (fetchvalue @args @old-subst)))

(def-sys-pred denyz ((uranus-user::? . uranus-user::?) . uranus-user::?)
  (denyz (fetchvalue @args @old-subst)))

(def-sys-pred denyq ((uranus-user::? . uranus-user::?) . uranus-user::?)
  (denyz (fetchvalue-q @args @old-subst)))

(def-sys-pred clear-all ()
  (p@clear-all))

;;; Clear all definitions.
(defun p@clear-all ()
       (do ((world @world-list (cdr world)))
	   ((null world) (setq @world-list '(standard-world)) t)
	 (mapcar
	   #'(lambda (x) (remprop x (car world)))
	   (get (car world) :world))
	 (remprop (car world) :world)))

(def-sys-multi-pred clause ((*assert *head . *body) *world)
  (clause (fetch '*assert @subst) @fetched-subst
	  (fetch '*head @subst) @fetched-subst
	  (fetch '*body @subst) @fetched-subst
	  (fetch '*world @subst) @fetched-subst
	  @uranus-world
	  nil)
  (unseen-world unseen-defs)
  (clause (fetch '*assert @old-subst) @fetched-subst
	  (fetch '*head @old-subst) @fetched-subst
	  (fetch '*body @old-subst) @fetched-subst
	  (fetch '*world @old-subst) @fetched-subst
	  unseen-world
	  unseen-defs)
  )

;;; The structure unseen-defs is used by clause.
(defstruct unseen-defs (deflist (ncons nil)) (current-pred nil) (unseen-preds nil))

(defun clause (as-flag a-subst pattern p-subst body b-subst world w-subst uranus-world defs)
  (cond ((varp pattern)
	 (cond ((assigned pattern p-subst)
		(clause as-flag a-subst (cadr @fetched-value) (cddr @fetched-value)
			body b-subst world w-subst uranus-world defs))
	       (t (clause2 as-flag a-subst pattern p-subst body b-subst
			   world w-subst
			   uranus-world defs))))
	((not (listp pattern)) (report-error "ILLEGAL ARGUMENT TO CLAUSE" Pattern))
	((varp (car pattern))
	 (cond ((not (varp (fetchvalue (car pattern) p-subst)))
		(clause as-flag a-subst
			(cons (fetchvalue (car pattern) p-subst)
			      (cdr pattern))
			p-subst
			body b-subst world w-subst uranus-world defs))
	       (t (clause2 as-flag a-subst pattern p-subst body b-subst
			   world w-subst
			   uranus-world defs))))
	((listp (car pattern)) (report-error "ILLEGAL ARGUMENT TO CLAUSE" Pattern))
	((varp (fetchvalue world w-subst))	;with unknown world
	 (clause0 as-flag a-subst pattern p-subst body b-subst world w-subst uranus-world defs))
	(t (clause1 as-flag a-subst pattern p-subst body b-subst
		    (fetchvalue world w-subst)
		    uranus-world defs))))	;with known world

;;; When world is unknown, searches through in the current nesting.
(defun clause0 (as-flag a-subst pattern p-subst body b-subst world w-subst uranus-world defs)
  (do (($world uranus-world (cdr $world))
	(undo @undo-point))
      ((null $world) nil)
    (if (and (find-clause as-flag a-subst nil pattern
			  defs
			  p-subst body b-subst (car $world))
	     (unify world w-subst (car $world) nil))
	(progn (setq unseen-world (if unseen-defs $world (cdr $world)))
	       (return t))
	(undo undo))))

;;; When the world is known, search in the world only.
(defun clause1 (as-flag a-subst pattern p-subst body b-subst world uranus-world defs)
  (and uranus-world				;if uranus-world is nil, it is the second time
						;or after
       (if (find-clause as-flag a-subst nil pattern
			defs
			p-subst body b-subst world)
	   (progn (setq unseen-world
			(if unseen-defs (ncons world) nil))
		  t)
	   nil)))

;;; When the predicate is unknown.
(defun clause2 (as-flag a-subst pattern p-subst body b-subst world w-subst uranus-world defs)
  (prog nil
	(if defs
	    (let ((undo @undo-point) ($world uranus-world))
	      (if (and(find-clause1 as-flag a-subst pattern defs p-subst body b-subst (car $world))
		      (unify world w-subst (car $world) nil))
		  (progn  (setq unseen-world
				(if unseen-defs $world (cdr $world)))
			  (return t))
		  (progn(undo undo)(setf defs nil) (pop uranus-world)))))
	(return
	  (let ((w (fetchvalue world w-subst)))
	    (cond ((varp w)			;then search all the nestings.
		   (do (($world uranus-world (cdr $world))
			(undo @undo-point))
		       ((null $world) nil)
		     (if (and (find-clause1 as-flag a-subst pattern defs p-subst body b-subst (car $world))
			      (unify world w-subst (car $world) nil))
			 (progn (setq unseen-world
				      (if unseen-defs $world (cdr $world)))
				(return t))
			 (undo undo))))
		  (uranus-world			;if uranus-world is nil, it is the second time
						;or after
		   (if (find-clause1 as-flag a-subst pattern defs p-subst body b-subst w)
		       (progn (setq unseen-world
				    (if unseen-defs (ncons w) nil))
			      t)
		       nil)))))))

(defun find-clause (as-flag a-subst pred pattern defs p-subst body b-subst $world)
  (let* ((flag (fetchvalue as-flag a-subst))
	 (name (or pred (car (fetchvalue pattern p-subst 2))))
	 (def (if defs
		  (unseen-defs-deflist defs)
		  (get name $world)))
	 (unseen (if defs (unseen-defs-unseen-preds defs) nil)))
    (cond ((varp flag)				;Try positive -> negative
	   (cond ((find-positive-clause
		    name (cdr def) (car def) unseen pattern p-subst body b-subst $world)
		  (unify 'uranus-user::assert nil as-flag a-subst))
		 ((find-negative-clause
		    name (cdr def) (car def) unseen pattern p-subst body b-subst $world)
		  (unify 'uranus-user::deny nil as-flag a-subst))))
	  ((eq flag 'uranus-user::assert)
	   (find-positive-clause
	     name (cdr def) nil unseen pattern p-subst body b-subst $world))
	  ((eq flag 'uranus-user::deny)
	   (find-negative-clause
	     name nil (car def) unseen pattern p-subst body b-subst $world))
	  (t (report-error "UNKNOWN CLAUSE HEAD" flag)))))

(defun find-positive-clause (pname pdef ndef unseen pattern p-subst body b-subst $world)
  (do ((d pdef (cdr d))
       (u @undolist)				;Variables in the head
						;must be undone, too.
       (h-subst (newsubst '||)))
      ((or (null d) (null (car d))) nil)
      (if (and (unify-q-q pattern p-subst
			  (cons pname (caar d)) h-subst)
	       (unify-q-q body b-subst (cdar d) h-subst))
	  (return (p@update-unseen-defs pname ndef (cdr d) unseen $world))
	(undo u))))

(defun find-negative-clause (pname pdef ndef unseen pattern p-subst body b-subst $world)
  (do ((d ndef (cdr d))
       (u @undolist)				;Variables in the head
						;must be undone, too.
       (h-subst (newsubst '||)))
      ((or (null d) (null (car d))) nil)
    (if (and (unify-q-q pattern p-subst
			(cons pname (caar d)) h-subst)
	     (unify-q-q body b-subst (cdar d) h-subst))
	(return (p@update-unseen-defs pname (cdr d) pdef unseen $world))
	(undo u))))

(defun p@update-unseen-defs (pname ndef pdef unseen $world)
  (setq unseen-defs
	(cond ((or ndef pdef)
	       (make-unseen-defs :current-pred pname
				 :deflist (cons ndef pdef)
				 :unseen-preds unseen))
	      (unseen
	       (make-unseen-defs :current-pred (car unseen)
				 :deflist (get (car unseen) $world)
				 :unseen-preds (cdr unseen)))
	      (t nil)))
  t)

;;; Predicate is unknown, world is given.

(defun find-clause1 (as-flag a-subst pattern defs p-subst body b-subst $world)
  (if defs
      (or (find-clause as-flag a-subst
		       (unseen-defs-current-pred defs)
		       pattern
		       defs
		       p-subst body b-subst $world)
	  (do ((pred (unseen-defs-unseen-preds defs) (cdr pred)))
	      ((null pred) nil)
		(if (find-clause as-flag a-subst
				(car pred)		;predicate name
				pattern
				(make-unseen-defs :deflist (get (car pred) $world)
						  :unseen-preds (cdr pred))
				p-subst body b-subst $world)
		  (return t))))
      (do ((pred (get $world :world) (cdr pred)))
	  ((null pred) nil)
	  (if (find-clause as-flag a-subst
			   (car pred)		;predicate name
			   pattern
			   (make-unseen-defs :deflist (get (car pred) $world)
					     :unseen-preds (cdr pred))
			   p-subst body b-subst $world)
	      ;must be undone, too.
	      (return t)))))

(DEF-SYS-PRED DEFINE (*NAME . *DEF)
  (p@define (FETCHVALUE '*NAME @SUBST) (FETCHVALUE '*def @subst)))

(DEF-SYS-PRED DEFINEq (*NAME . *DEF)
  (p@define (FETCHVALUE '*NAME @SUBST) (FETCHVALUE-q '*def @subst)))

(DEF-SYS-PRED DEFINE* (*NAME *pos *neg)
  (p@define* (FETCHVALUE '*NAME @SUBST)
	    (FETCHVALUE '*pos @subst) (FETCHVALUE '*neg @subst)
	    (car @uranus-world)))

(DEF-SYS-PRED DEFINEq* (*NAME *pos *neg)
  (p@define* (FETCHVALUE '*NAME @SUBST)
	    (FETCHVALUE-q '*pos @subst) (FETCHVALUE-q '*neg @subst)
	    (car @uranus-world)))

(defun p@define (pred defs)
    ;;; Old format : (DEFINE pred (arg1 . body1) (arg2 . body2) ... )
    ;;; New format : (DEFINE pred :NAGATIVE (arg-n1 . body-n1) ...
    ;;;                           :POSITIVE (arg1 . body1) (arg2 . body2) ... ))
    ;;; The following code takes care of both.
  (p@define* pred (r@positive-part defs) (r@negative-part defs) (car @uranus-world)))
(defun p@define* (pred pos neg world)
  (if (symbolp pred)
      (progn (setf (get pred world)
		   (cons neg
			 (nconc pos '(nil))))
		   ;the last NIL prevents further search
	     (p@register-into-world world pred))
      (report-error "ILLEGAL ARGUMENT TO DEFINE" pred)))


(defun r@positive-part (defs)
  (let ((p (memq :positive defs)))
    (if p
	(if (memq :negative p)
	    (stop-before-negative (cdr p))
	    (cdr p))
	(if (memq :negative defs)
	    nil
	    defs))))

(defun stop-before-negative (x)
  (if (eq (car x) :negative) nil
      (cons (car x) (stop-before-negative (cdr x)))))

(defun r@negative-part (defs)
  (let ((p (memq :negative defs)))
    (if p
	(if (memq :positive p)
	    (stop-before-positive (cdr p))
	    (cdr p))
	nil)))

(defun stop-before-positive (x)
  (if (eq (car x) :positive) nil
      (cons (car x) (stop-before-positive (cdr x)))))


(def-sys-pred definition (*name *def . *ndef)
  (unify (cdr (get (fetch '*name @subst) (car @uranus-world)))
	 (newsubst)
	 '*def
	 @subst)
  (if (null (fetchvalue '*ndef @subst))
      t
      (unify (ncons (car (get (fetch '*name @subst) (car @uranus-world))))
	     (newsubst)
	     '*ndef
	     @subst)))
(def-sys-pred definitionq (*name *def . *ndef)
  (unify-q-n (cdr (get (fetch '*name @subst) (car @uranus-world)))
	     (newsubst)
	     '*def
	     @subst)
  (if (null (fetchvalue '*ndef @subst))
      t
      (unify-q-n (ncons (car (get (fetch '*name @subst) (car @uranus-world))))
		 (newsubst)
		 '*ndef
		 @subst)))


(DEF-SYS-PRED (LISTING LIS) *ARGS
  (p@listing (FETCH '*ARGS @SUBST) @fetched-subst))

(defun p@listing (preds @subst)
  (MAPC #'(LAMBDA (NAME)
	    (COND ((and (varp name) (assigned name @subst))
		   (p@list-neg-pos (fetch name @subst)
				   (r@getdef (ncons (fetch NAME @subst)) @uranus-world)))
		  ((SYMBOLP NAME)
		   (p@list-neg-pos name (r@getdef (ncons NAME) @uranus-world)))
		  (T (REPORT-ERROR '"ILLEGAL ARGUMENT TO LISTING" NAME))))
	preds))
  
(defun p@list-neg-pos (name neg-pos)
  (MAPC #'(LAMBDA (D)
	    (cond ((null d) (print nil standard-output))
		  (t (Pprint 
		       `(Uranus-User::deny (,name . ,(car d)) . ,(cdr d))
		       standard-output))))
	(car neg-pos))
  (MAPC #'(LAMBDA (D)
	    (cond ((null d) (print nil standard-output))
		  (t (Pprint 
		       `(Uranus-User::assertq (,name . ,(car d)) . ,(cdr d))
		       standard-output))))
	(cdr neg-pos)))
(def-sys-pred retract (*name . *body)
  (retract (fetch '*name @subst) @fetched-subst (fetch '*body @subst) @fetched-subst))

(DEF-SYS-PRED SET (*NAME . *VALUE)
  (let ((pred (FETCHVALUE '*NAME @SUBST)))
    (setf (get pred (CAR @URANUS-WORLD))
	  (cons nil
		(NCONS (NCONS (FETCHVALUE (CDR @ARGS)
					  (cons (CONS '"" (CDaR @OLD-SUBST))
						(CDR @OLD-SUBST)))))))
    (P@REGISTER-INTO-WORLD (CAR @URANUS-WORLD) pred)
    t))

;;;
;;; I/O Predicates
;;;

(DEF-SYS-PRED ADD (*FILE . *NAMES)
  (LET ((FN (INTERN (FETCHVALUE '*FILE @SUBST))) 
	(X (FETCHVALUE '*NAMES @SUBST)))
    (PRINT (setf (get FN 'loaded) (UNION X (GET FN 'LOADED))))))

(def-sys-pred clear-input *file
  (let ((file (fetchvalue '*file @subst)))
    (clear-input (if file (car file) terminal-input))
    t))

(def-sys-pred clear-output *file
  (let ((file (fetchvalue '*file @subst)))
    (clear-output (if file (car file) terminal-input))
    t))

(DEF-SYS-PRED DEL (*FILE . *NAMES)
  (LET ((FN (INTERN (FETCHVALUE '*FILE @SUBST))) 
	(NAMES (FETCHVALUE '*NAMES @SUBST)))
    (PRINT (setf (get FN 'loaded)
		 (LET ((LOADED (GET FN 'LOADED)))
		   (MAPC (FUNCTION (LAMBDA (X) (SETQ LOADED (DELQ X LOADED))))
			 NAMES)
		   LOADED)))))

(DEF-SYS-PRED DUMP (*FILE *NAMES)
  (P@DUMP (FETCHVALUE '*FILE @SUBST) (FETCHVALUE '*NAMES @SUBST)))

(DEFUN P@DUMP (FILENAME NAMES)
  (PROG (FLIST STANDARD-OUTPUT FILE pathname)
    (SETQ FLIST 
          (COND ((SYMBOLP NAMES) (GET NAMES :WORLD))
                ((LISTP NAMES) NAMES)
                (T NIL)))
    (setq pathname (merge-pathnames
		     filename
		     #+symbolics(merge-pathnames (fs:uranus-user-homedir) @default-pathname)
		     #-symbolics @default-pathname))
    (COND (FLIST (setf (get (pathtosym pathname) 'loaded) FLIST))
          (T (report-error "NOTHING TO BE DUMPED" NAMES) (RETURN T)))
    (SETQ STANDARD-OUTPUT
	  (SETQ FILE
		(OPEN pathname :direction :output #+vax :if-exists #+vax :overwrite)))

    (format standard-output ";;; -*- Mode: Uranus; Base:10 -*-~%")
    (format standard-output ";;; File created from Uranus system on:~%")
#+symbolics    (format standard-output ";;; ~\date\~3%" (time::get-universal-time))

    (COND ((ATOM FLIST)
           (report-error "ILLEGAL ARGUMENT TO DUMP" FLIST)
           (RETURN T))
          (T (LET ((@URANUS-WORLD
                    (COND ((SYMBOLP NAMES) (CONS NAMES @URANUS-WORLD))
                          (T @URANUS-WORLD))))
               (print@defs flist (car @uranus-world)))))
    (CLOSE FILE)
    (RETURN T)))



(DEF-SYS-PRED PRIND (*X) (PROG1 T (PPRINT (FETCHVALUE '*X @SUBST) standard-output)))

(DEF-SYS-PRED LOAD *FILE (P@LOAD (FETCHVALUE '*FILE @SUBST)))

(DEF-SYS-PRED ReLOAD *FILE
  (let ((@RELOAD-FLAG T)
	(@RELOAD-DEFLIST NIL))
    (P@LOAD (FETCHVALUE '*FILE @SUBST))))

(DEF-SYS-PRED LOAD-WORLD (*FILE *WORLD)
  (LET ((@URANUS-WORLD (CONS (FETCHVALUE '*WORLD @SUBST) @URANUS-WORLD)))
    (P@LOAD (FETCHVALUE '(*FILE) @SUBST))))

(DEFUN P@LOAD (FILE)
   (LET* ((pathname (if file
		     (merge-pathnames
		       (car file)
		       @default-pathname)
		     #+symbolics(merge-pathnames (fs:uranus-user-homedir) @default-pathname)
		     #-symbolics @default-pathname))
	  (LOADED NIL)
	  (@basic-world @uranus-world)		; basic-world may be changed.
	  (stream (open pathname :if-does-not-exist nil)))
       (if (not stream)
	 (report-error "FILE NOT FOUND" (namestring pathname))
	(DO ((X (READ stream nil 'EOF) (READ stream nil 'EOF))
	     (@BACKTRACE NIL NIL)
	     (@uranus-world @basic-world @basic-world))
	    ((EQ X 'EOF)
	     (close stream)
	     (setf (get (pathtosym pathname) 'loaded) (nreverse loaded))
	     )
	  (EXECUTE X nil)
	  (COND ((EQ (CAR X) 'uranus-user::DEFINE) (PUSH (CADR X) LOADED))
		((MEMQ (CAR X) '(uranus-user::ASSERT uranus-user::AS 
					     uranus-user::assertz uranus-user::az uranus-user::asserta uranus-user::aa
					     uranus-user::assertq uranus-user::aq uranus-user::deny))
		 (SETQ LOADED (UNION (NCONS (CAR (CADR X))) LOADED)))
		(T (PUSH X LOADED)))))))

(DEF-SYS-PRED LOADED (*FILE *LOADED)
  (UNIFY '*LOADED
	 @SUBST
	 (get (pathtosym
	       (merge-pathnames
		(FETCHVALUE '*FILE @SUBST)
		#+symbolics(merge-pathnames (fs:uranus-user-homedir) @default-pathname)
		#-symbolics @default-pathname))
	      'LOADED)
	 @SUBST))

(DEF-SYS-PRED PRINT (*X . *Y)
  (LET* ((ARGS (FETCHVALUE '*Y @SUBST))
	 (stream (AND ARGS (CAR ARGS)))
	 (STANDARD-OUTPUT
	   (COND ((eq stream 'terminal-output) terminal-output)
		 (stream)
		 (T STANDARD-OUTPUT))))
    (and args (cdr args) (report-error "TOO MANY ARGUMENTS TO PRINT" args))
    (print (FETCHVALUE (fetch '*X @SUBST) @fetched-subst @PRINTLEVEL)
	   standard-output)
    T))

(DEF-SYS-PRED PRIN1 (*X . *Y)
  (LET* ((ARGS (FETCHVALUE '*Y @SUBST))
	 (stream (AND ARGS (CAR ARGS)))
	 (STANDARD-OUTPUT
	   (COND ((eq stream 'terminal-output) terminal-output)
		 (stream)
		 (T STANDARD-OUTPUT))))
    (and args (cdr args) (report-error "TOO MANY ARGUMENTS TO PRIN1" args))
    (prin1 (FETCHVALUE (fetch '*X @SUBST) @fetched-subst @PRINTLEVEL)
	   standard-output)
    T))

(DEF-SYS-PRED PRINC (*X . *Y)
  (LET* ((ARGS (FETCHVALUE '*Y @SUBST))
	 (stream (AND ARGS (CAR ARGS)))
	 (STANDARD-OUTPUT
	   (COND ((eq stream 'terminal-output) terminal-output)
		 (stream)
		 (T STANDARD-OUTPUT))))
    (and args (cdr args) (report-error "TOO MANY ARGUMENTS TO PRINC" args))
    (PRINc (FETCHVALUE (fetch '*X @SUBST) @fetched-subst @PRINTLEVEL)
	   standard-output)
    T))

(DEF-SYS-PRED PRINT-LEVEL (*LEVEL)
  (LET ((L (FETCHVALUE '*LEVEL @SUBST)))
    (COND ((varp l) (unify '*level @subst @printlevel @subst))
	  ((NUMBERP L) (SETQ @PRINTLEVEL L))
	  (T (REPORT-ERROR '"ILLEGAL ARGUMENT TO PRINT-LEVEL" L)))))

(def-sys-pred line-length (*length)
  (LET ((L (FETCHVALUE '*LENGTH @SUBST)))
    (COND ((varp l) (unify '*length @subst line-length @subst))
	  ((NUMBERP L) (SETQ line-length L))
	  (T (REPORT-ERROR '"ILLEGAL ARGUMENT TO LINE-LENGTH" L)))))

(DEF-SYS-PRED READ (*X . *STREAM)
  (UNIFY '*X
	 @SUBST
	 (LET ((STANDARD-INPUT
		 (LET ((S (FETCHVALUE '*STREAM @SUBST)))
		   (COND ((null s) standard-input)
			 ((eq (car s) 'terminal-input) terminal-input)
			 (t (car s))))))
	   (READ STANDARD-INPUT))
	 (newsubst '||)))

(DEF-SYS-PRED FREAD (*X . *STREAM)                       ;FREAD reads an
  (UNIFY '*X					;expression in the
	 @SUBST					;current environment.
	 (LET ((STANDARD-INPUT
		 (LET ((S (FETCHVALUE '*STREAM @SUBST)))
		   (COND ((null s) standard-input)
			 ((eq (car s) 'terminal-input) terminal-input)
			 (t (car s))))))
	   (READ STANDARD-INPUT))
	 @old-subst))

(def-sys-pred standard-input (*stream)
  (let ((stream (fetchvalue '*stream @subst)))
    (cond ((varp stream)
	   (unify '*stream @subst standard-input @subst))
;	  ((symbolp stream)
;	   (setq standard-input (eval stream)))
	  (t (setq standard-input stream)))))

(def-sys-pred standard-output (*stream)
  (let ((stream (fetchvalue '*stream @subst)))
    (cond ((varp stream)
	   (unify '*stream @subst standard-output @subst))
;	  ((symbolp stream)
;	   (setq standard-output (eval stream)))
	  (t (setq standard-output stream)))))

(DEF-SYS-PRED STORE (*FILE) (P@STORE (FETCHVALUE '*FILE @SUBST)))

(DEFUN P@STORE (FILENAME)
   (PROG (FLIST STANDARD-OUTPUT FILE pathname)
	 (setq pathname (merge-pathnames
			  filename
			  #+symbolics(merge-pathnames (fs:user-homedir) @default-pathname)
			  #-symbolics @default-pathname))
	 (SETQ FLIST (get (pathtosym pathname) 'LOADED))
	 (COND (FLIST) (T (report-error '"NOT LOADED YET" FILENAME) (RETURN T)))
	 (SETQ STANDARD-OUTPUT
	       (SETQ FILE (OPEN pathname :direction :output)))
	 (COND ((ATOM FLIST)
		(report-error '"ILLEGAL ARGUMENT TO STORE" FLIST)
		(RETURN T))
	       (T (print@defs flist (car @uranus-world))))
	 (CLOSE FILE)
	 (RETURN T)))

(DEF-SYS-PRED TERPRI *file
	      (prog1 t
		(terpri (or (car (fetchvalue '*file @subst)) standard-output))))

(def-sys-pred tyi (*char . *file)
  (unify '*char @subst
	 (read-char (or (car (fetchvalue '*file @subst)) standard-output))
	 nil))

(def-sys-pred tyo (*char . *file)
  (write-char (fetchvalue '*char @subst)
	      (or (car (fetchvalue '*file @subst)) standard-output)))


;;;
;;; Initialize  control predicates
;;;


(DEF-SYS-PRED ACCUMULATE (URANUS-USER::? URANUS-USER::? URANUS-USER::?)
  (P@ACCUMULATE (CAR @ARGS) (CADR @ARGS) (CADDR @ARGS) @OLD-SUBST))

(DEFUN P@ACCUMULATE (STRUCTURE FORM VARIABLE $SUBST)
  (with-new-env form nil
		(do ((RESULT NIL))
		    ((NOT (CATCH :REFUTE
			    (AND (refute-one-loop)
				 (PROGN (PUSH (FETCHVALUE STRUCTURE $SUBST)
					      RESULT)
					(R@FAIL)
					T))))
		     (UNIFY VARIABLE $SUBST (NREVERSE RESULT) @SUBST)
		     ))))

(DEF-SYS-PRED AND *ARGS
  (LET ((ARGS (FETCH '*ARGS @SUBST))
	(@level (1+ @level)))
    (AND ARGS (r@push-continuation ARGS @FETCHED-SUBST))
    T))

(DEF-SYS-PRED CANDIDATES URANUS-USER::? (P@CANDIDATES (CAR @ARGS) (CDR @ARGS) @OLD-SUBST))


(DEFUN P@CANDIDATES (VARIABLE $CLAUSE $SUBST)
   (COND ((VARP VARIABLE)
	  (with-new-env (car $clause) (cdr $clause)
			(do ((RESULT NIL))
			    ((NOT (CATCH :REFUTE
				    (AND (refute-one-loop)
					 (PROGN (PUSH (FETCHVALUE VARIABLE $SUBST) 
						      RESULT)
						(R@FAIL)
						T))))
			     (UNIFY VARIABLE $SUBST (NREVERSE RESULT) $SUBST)
			     ))))
         (T (REPORT-ERROR '"ILLEGAL ARGUMENT TO CANDIDATES" VARIABLE))))

(DEF-SYS-PRED CATCH (*PRED)
  (let* (($cue @cue)
	 ($stack @stack)
	 (form-subst
	   (catch :catch
	     (progn (r@push-continuation
		      (ncons (fetch '*pred @subst))
		      @fetched-subst)
		    (r@pop-continuation)
		    (setq @level (1+ @level))
		    (do nil (nil) (refute@one)))))
	 (form (car form-subst))
	 (subst (cdr form-subst)))
    (cond ((unify '*pred @subst form subst)
	   (setq @cue $cue @stack $stack)
	   t)
	  (t (throw ':catch form-subst)))))

(def-sys-pred cut nil (prog1 t (setq @stack  @father)))

(def-sys-pred local-cut nil
  (prog1 t (setq @stack @mother)))

(DEF-SYS-PRED COND *ARGS
  (LET ((A (FETCH '*ARGS @SUBST)) ($SUBST @FETCHED-SUBST))
    (DO ((W))
	((NULL A))
      (COND ((ATOM (SETQ W (POP A)))
	     (REPORT-ERROR '"ILLEGAL ARGUMENT TO COND" W))
	    ((REFUTE (CAR W) $SUBST (1+ @LEVEL))
	     (RETURN (COND ((CDR W) (r@push-continuation (CDR W) $SUBST))
			   (T))))))))

(DEF-SYS-PRED DAND URANUS-USER::?
  (DO ((A @ARGS (CDR A)))
      ((NULL A) T)
    (COND ((REFUTE (CAR A) @old-SUBST (1+ @LEVEL))) (T (RETURN NIL)))))

(DEF-SYS-PRED DO *body (P@DO (fetch '*body @SUBST) @fetched-subst))

(DEF-SYS-PRED DOR *ARGS
  (DO ((A @ARGS (CDR A)))
      ((NULL A))
    (AND (REFUTE (CAR A) @OLD-SUBST (1+ @LEVEL) NIL) (RETURN T))))

(DEF-SYS-PRED FAIL nil (setq @stack  @father) nil)
  
(DEF-SYS-internal $FAIL$ (*FORM) NIL)
   
(DEF-SYS-PRED FALSE NIL NIL)

(DEF-SYS-PRED FOR-ALL (*PRED . *BODY)
  (P@FOR-ALL
    (FETCH '*PRED @SUBST)
    (FETCH '*BODY @SUBST)
    @FETCHED-SUBST))
  
(DEFUN P@FOR-ALL (FOR-PRED DO-PRED $SUBST)
   (CATCH :FOR-ALL
     (with-new-env for-pred nil
		   (do ()
		       ((NOT (CATCH :REFUTE
			       (AND (refute-one-loop)
				    (COND ((REFUTE@N DO-PRED $SUBST (1+ @LEVEL))
					   (R@FAIL) T)
					  (T (THROW :FOR-ALL NIL))))))
			T)))))


(DEF-SYS-PRED IF (*IF *THEN . *ELSE)
  (COND ((REFUTE (FETCH '*IF @SUBST) @FETCHED-SUBST (1+ @LEVEL) NIL)
	 (r@push-continuation (NCONS (FETCH '*THEN @SUBST)) @FETCHED-SUBST))
	((FETCH '*ELSE @subst)
	 (r@push-continuation (FETCH '*ELSE @SUBST) @FETCHED-SUBST))
	(T)))

(DEF-SYS-PRED INITIATE (*FORM *NAME)
  (UNIFY '*NAME
	 @SUBST
	 (P@INITIATE (FETCH '*FORM @SUBST) @FETCHED-SUBST)
	 @SUBST))

(DEF-SYS-PRED LOOP URANUS-USER::? (CATCH :LOOP (P@LOOP @ARGS @OLD-SUBST)))

(DEF-SYS-PRED exit NIL
  (or (errset (THROW :LOOP T) nil)
      (REPORT-ERROR '"EXIT USED OUTSIDE LOOP")))

(DEF-SYS-PRED NEXT (*NAME . *ARGS)
  (P@NEXT (FETCHVALUE '*NAME @SUBST)
    (FETCH '*ARGS @SUBST)
    @FETCHED-SUBST))

(DEF-SYS-PRED NOT URANUS-USER::? 
  (NOT (REFUTE@N (fetch @ARGS @OLD-SUBST)
	      @fetched-subst
	      (1+ @LEVEL))))

(DEF-SYS-PRED ONBT (*PRED)
  (LET ((@LEVEL (1+ @LEVEL)))
    (r@push-alternatives (LIST 'AND (FETCH '*PRED @SUBST) '(FAIL))
		  NIL
		  nil
		  @FETCHED-SUBST)))

(DEF-SYS-PRED OR *ARGS
  (LET (($ARGS (FETCH '*ARGS @SUBST))
	($SUBST @FETCHED-SUBST)
	(@level (1+ @level)))
    (COND ($ARGS (AND (CDR $ARGS)
		      (r@push-alternatives (CONS 'OR (CDR $ARGS)) NIL nil @old-subst))
		 (r@push-continuation (NCONS (CAR $ARGS)) $SUBST)
		 T)
	  (T NIL))))

(DEF-SYS-PRED RETURN *ARGS
  (progn (setq @stack @father)
	 (r@push-continuation (fetch '*args @subst) @fetched-subst)
	 t))

(DEF-SYS-PRED THROW (*PATTERN) (P@THROW '*PATTERN @SUBST))

(DEF-SYS-PRED TRUE nil t)

;;;
;;; Miscellaneout predicates
;;;


(DEF-SYS-PRED ATOM (*X)
  (LET ((V (FETCHVALUE '*X @SUBST))) (AND (ATOM V) (NOT (VARP V)) (not (td-p v)))))

(def-sys-pred copy (*o *c)
  (unify '*c @subst (fetchvalue-q '*o @subst) @old-subst))

(DEF-SYS-PRED CREATE-WORLD (*WORLD . *DEFS)
  (P@WORLD (FETCHVALUE '*WORLD @SUBST) (FETCHVALUE '*DEFS @SUBST)))

(DEF-SYS-PRED EDIT (*NAME . *COMMANDS)
  (P@EDIT (FETCHVALUE '*NAME @SUBST)
    (FETCHVALUE
      (FETCH '*COMMANDS @SUBST)
      (cons (CONS '"" (CDaR @FETCHED-SUBST)) (CDR @FETCHED-SUBST)))))

(DEF-SYS-PRED EDITS (*NAME . *COMMANDS)
  (P@EDIT (FETCHVALU '*NAME @SUBST)
          (FETCHVALUE
           (FETCH '*COMMANDS @SUBST)
           (cons (CONS '"" (CDaR @FETCHED-SUBST)) (CDR @FETCHED-SUBST)))
          nil))

(DEF-SYS-PRED GT (*X *Y)
  (> (FETCHVALUE '*X @SUBST) (FETCHVALUE '*Y @SUBST)))

(DEF-SYS-PRED LT (*X *Y)
  (< (FETCHVALUE '*X @SUBST) (FETCHVALUE '*Y @SUBST)))

(DEF-SYS-PRED LAST-RESULT (*R)
  (UNIFY '*R @SUBST (CAR @LAST-RESULT) (CDR @LAST-RESULT)))

#-lucid(DEF-SYS-PRED (quit EPILOG) NIL 
  (THROW :LISP 'EPILOG))

#+lucid(def-sys-pred (bye quit epilog) nil
	 (lucid::quit))

(DEF-SYS-PRED (EQ ==) (*X *Y)
  (LET ((X (FETCHvalue '*x @SUBST)) 
	(XS @FETCHED-SUBST) 
	(Y (FETCHvalue '*Y @SUBST)) 
	(YS @FETCHED-SUBST))
    (AND (EQual X Y) (EQ XS YS))))

(DEF-SYS-PRED (EQq =eq=) (uranus-user::? uranus-user::?)
  (LET ((X (FETCHvalue-q (first @args) @old-SUBST)) 
	(XS @FETCHED-SUBST) 
	(Y (FETCHvalue-q (second @args) @old-SUBST)) 
	(YS @FETCHED-SUBST))
    (AND (EQual X Y) (EQ XS YS))))

(DEF-SYS-PRED (MATCH =) (Uranus-User::? Uranus-User::?) 
  (unify (first @args) @old-subst (second @args) @old-subst))

(def-sys-pred (matchq =q=) (uranus-user::? uranus-user::?)
  (unify-q-q (first @args) @old-subst (second @args) @old-subst))

(def-sys-pred metize (*x *y)
  (unify (metize '*x @subst) @subst '*y @subst))

(def-sys-pred metap (uranus-user::?)
  (let ((form (fetch (first @args) @old-subst)))
    (and (td-p form)
	 (not (= (td-level form) 0)))))

(def-sys-pred meta-level (uranus-user::? *n)
  (unify (let ((form (fetch (first @args) @old-subst)))
	   (if (td-p form)
	       (td-level form)
	       0))
	 @subst
	 '*n 
	 @subst))

(def-sys-pred objectize (*x *y)
  (unify (fetchvalue '*x @subst) @subst '*y @subst))

(DEF-SYS-PRED PRED (*X *Y)
  (UNIFY '*Y @SUBST (1- (FETCHVALUE '*X @SUBST)) @subst))

(DEF-SYS-PRED SELECT (URANUS-USER::? . URANUS-USER::?)
  (DO ((PAT (CAR @ARGS)) (SELECT (CDR @ARGS) (CDR SELECT)) (U @UNDOLIST))
      ((NULL SELECT) NIL)
    (COND ((UNIFY PAT @OLD-SUBST (CAAR SELECT) @OLD-SUBST)
	   (and (cdar select)			;Push the rest only
		(r@push-continuation (CDAR SELECT) @OLD-SUBST))	;when non-nil.
	   (RETURN T))
	  (T (UNDO U)))))

(DEF-SYS-PRED SUCC (*X *Y)
  (UNIFY '*Y @SUBST (1+ (FETCHVALUE '*X @SUBST)) @subst))

(def-sys-pred systemp (*name)
  (systemp (fetchvalue '*name @subst)))

(defun systemp (name)
  (or (r@get-pred-value name :code)
      (fboundp name)))

(def-sys-pred (term-description td) (*x)
  (td-p (fetchvalue-q '*x @subst)))

(def-sys-pred time (*PRED . *TIME)
  (if (null (fetchvalue-q '*time @subst 1))
      (time (REFUTE (FETCH '*PRED @SUBST) @FETCHED-SUBST @LEVEL NIL))
    (LET* ((TM1 (get-internal-real-time))
	   (RS (REFUTE (FETCH '*PRED @SUBST) @FETCHED-SUBST @LEVEL NIL))
	   (tm2 (get-internal-real-time)))
      (declare (ignore RS))
      (PROG1 (UNIFY '*TIME 
		    @SUBST 
		    (ncons (/ (- tm2 tm1)
			      internal-time-units-per-second 1.0))
		    @subst)
	     ))))
;		 (AND (EQ (CAR @GOAL) 'TIME)
;		      (SETQ @GOAL (LIST 'TIME RS (FETCH '*TIME @SUBST))))))))

(DEF-SYS-PRED VAR (*X)
  (VARP (FETCHvalue-q '*X @SUBST 1)))

(DEF-SYS-PRED VERSION NIL (VERSION))

(DEF-SYS-PRED WITH (*world . *PRED)
  (P@WITH (FETCHVALUE '*world @SUBST)
    (FETCH '*PRED @SUBST)
    @FETCHED-SUBST))

(def-sys-pred within (*world . *pred)
  (p@within (fetchvalue '*world @subst)
    (fetch '*pred @subst)
    @fetched-subst))

(DEF-SYS-PRED ERASE-WORLD (*NAME) 
  (P@ERASE-WORLD (FETCHVALUE '*NAME @SUBST)))

(DEF-SYS-PRED PEEK-WORLD (*NAME *LIST)
  (UNIFY (GET (FETCHVALUE '*NAME @SUBST) :WORLD) @SUBST '*LIST @SUBST))

(def-sys-pred push-world (*world)
  (let ((world (fetchvalue '*world @subst)))
    (cond ((symbolp world)
	   (push world @uranus-world)
	   (push world @basic-world))
	  (t (report-error '"ILLEGAL WORLD NAME" world)))))

;;; pop-world has effects only at the top-level.
;;; This is because @uranus-world is saved in the stack, and there is no way
;;; to change it.
(def-sys-pred pop-world ()
  (and (cdr @basic-world) (pop @basic-world)))

(def-sys-pred current-world (*world)
  (unify '*world @subst @uranus-world @subst))

(def-sys-pred universe (*world-list)
  (unify '*world-list @subst @world-list @subst))

(def-sys-pred reset-world *world-list
  (let ((worlds (fetchvalue '*world-list @subst)))
    (cond ((null worlds) (setq @uranus-world '(Uranus-User::standard-world)))
	  (t (setq @uranus-world worlds)))))

(DEF-SYS-PRED X *ARGS (SETQ @GOAL (EVAL (FETCHVALUE '*ARGS @SUBST))))

;;;
;;; Error handling predicats
;;;
(DEF-SYS-PRED ATTENTION NIL
  (LET ((STANDARD-OUTPUT terminal-output)
	(standard-input terminal-input)
	(@error-or-attention T))
    (URANUS-STEP)
    t))

(DEF-SYS-PRED BACKTRACE (*BT) 
  (UNIFY '*BT @SUBST (p@BACKTRACE 0) @SUBST))

(DEF-SYS-PRED DEBUG (*FLAG)
  (prog1 t
	 (case (FETCHVALUE '*FLAG @SUBST)
	   (uranus-user::ON (SETQ @DEBUG1 T))
	   (uranus-user::OFF (SETQ @DEBUG1 NIL))
	   (t (UNIFY '*FLAG @SUBST (COND (@DEBUG1 'URANUS-USER::ON) (T 'URANUS-USER::OFF)) @SUBST)))
	 (update-debug-flag)))

(DEF-SYS-PRED URANUS NIL 
  (PGO))

(DEF-SYS-PRED (ERROR standard-error-handler) (*STATEMENT *AT)
  (URANUS-BREAK))

(DEF-SYS-PRED STEP (*PRED . *SELECT)
  (LET ((@DEBUG T) (@TRACEALL NIL) (@TRACE NIL)
	(@SELECTSTEP (fetchvalue '*select @subst))
	(@STEPLEVEL 99999.))
    (LET ((PRED (FETCH '*PRED @SUBST)))
      (cond
	((varp pred) (unify '*pred @subst
			    (cond (@step 'uranus-user::on) (t 'uranus-user::off)) @subst))
	((eq pred 'URANUS-USER::ON) (make-step-on))
	((eq pred 'URANUS-USER::OFF) (make-step-off))
	(T (LET ((@STEP (null @selectstep))) (REFUTE PRED @FETCHED-SUBST)))))
    t))

(defun make-step-on () (SETQ @STEP T) (setq @debug t))
(defun make-step-off ()  (SETQ @STEP NIL)
	 (update-debug-flag))

(DEF-SYS-PRED TOP NIL (THROW :TOP T))

(DEF-SYS-PRED TRACE *ARGS
  (SETQ @TRACE (APPEND (FETCHVALUE '*ARGS @SUBST) @TRACE)))

(DEF-SYS-PRED TRACE-ALL NIL
  (prog1 t
	 (SETQ @TRACEALL T)
	 (update-debug-flag)))

(DEF-SYS-PRED UNTRACE *ARGS
  (PROG1 T
	 (MAPC (FUNCTION (LAMBDA (X) (SETQ @TRACE (DELQ X @TRACE))))
	       (FETCHVALUE '*ARGS @SUBST))
	 (update-debug-flag)))

(DEF-SYS-PRED UNTRACE-ALL NIL
  (PROG1 T (SETQ @TRACEALL NIL) (SETQ @TRACE NIL) (SETQ @TRACELEVEL NIL)
	 (update-debug-flag)))

(setf (get 'uranus-user::MEMBER nil)			; This definition is in the world "NIL".
      '(nil					;no negative information
	 ((uranus-user::*ELEMENT (uranus-user::*ELEMENT . uranus-user::*REST)))
	 ((uranus-user::*ELEMENT (uranus-user::*TOP . uranus-user::*REST))
	  (uranus-user::MEMBER uranus-user::*ELEMENT uranus-user::*REST))
	 NIL))					; to block failure proceeds to search Lisp function MEMBER.

(setf (get 'uranus-user::append nil)
	   '(nil
	     ((() uranus-user::*any uranus-user::*any))
	     (((uranus-user::*a . uranus-user::*x) uranus-user::*y (uranus-user::*a . uranus-user::*z))
	      (uranus-user::append uranus-user::*x uranus-user::*y uranus-user::*z))
	     nil))

;;; The P@Edit is the interface function to Amuse.

(proclaim '(special edit-world))

#+symbolics
(DEFUN P@EDIT (amuse::EDIT-NAME &OPTIONAL (COM NIL) (with-window t))
  (COND ((SYMBOLP AMUSE::EDIT-NAME)
	 (let ((edit-world))
	   (LET ((AMUSE::EDITOR-GET-DEFINITION
		   #'(LAMBDA (X) (AMUSE::FETCHVALUE (edit-get-uranus-def x))))
		 (AMUSE::EDITOR-RESTORE-DEFINITION
		   #'(LAMBDA (DEF)
		       (setf (get AMUSE::EDIT-NAME edit-world)
			     (AMUSE::FETCHVALUE DEF))))
		 (AMUSE::EDITOR-EXECUTE #'(LAMBDA (X) (EXECUTE X))))
	     (URANUS-USER::AMUSE AMUSE::EDIT-NAME COM with-window)
	     T)))
	 (T (LET ((AMUSE::EDITOR-GET-DEFINITION #'(LAMBDA (X) (AMUSE::GETFILE X)))
		  (AMUSE::EDITOR-RESTORE-DEFINITION
		    #'(LAMBDA (DEF) (AMUSE::PUTFILE AMUSE::EDIT-NAME DEF)))
		  (AMUSE::EDITOR-EXECUTE #'(LAMBDA (X) (EXECUTE X))))
	      (URANUS-USER::AMUSE AMUSE::EDIT-NAME COM with-window)
	      T))))
#-symbolics
(DEFUN P@EDIT (amuse::EDIT-NAME &OPTIONAL (COM NIL) (with-window t))
  (DECLARE (IGNORE WITH-WINDOW))
  (COND ((SYMBOLP AMUSE::EDIT-NAME)
	   (LET ((AMUSE::EDITOR-GET-DEFINITION
		   #'(LAMBDA (X) (AMUSE::FETCHVALUE (edit-get-uranus-def x))))
		 (AMUSE::EDITOR-RESTORE-DEFINITION
		   #'(LAMBDA (DEF)
		       (setf (get AMUSE::EDIT-NAME edit-world)
			     (AMUSE::FETCHVALUE DEF))))
		 (AMUSE::EDITOR-EXECUTE #'(LAMBDA (X) (EXECUTE X))))
	     (URANUS-USER::AMUSE AMUSE::EDIT-NAME COM nil)
	     T))
	 (T (LET ((AMUSE::EDITOR-GET-DEFINITION #'(LAMBDA (X) (AMUSE::GETFILE X)))
		  (AMUSE::EDITOR-RESTORE-DEFINITION
		    #'(LAMBDA (DEF) (AMUSE::PUTFILE AMUSE::EDIT-NAME DEF)))
		  (AMUSE::EDITOR-EXECUTE #'(LAMBDA (X) (EXECUTE X))))
	      (URANUS-USER::AMUSE AMUSE::EDIT-NAME COM nil)
	      T))))

(defun edit-get-uranus-def (name)
  (cond ((listp name)
	 (get (first name) (setq edit-world (second name))))
	((GET name (setq edit-world (CAR @URANUS-WORLD))))
	((symbolp name)
	 (do ((plist (symbol-plist name))
	      (world nil))
	     ((null plist)
	      (cond ((null world) nil)
		    (t (format t "~A is defined in worlds ~A
Which world? " name world)
		       (get name (setq edit-world (read))))))
	   (cond ((get (car plist) :world) (push (pop plist) world))
		 (t (pop plist)))
	   (pop plist)))))

;;;  defs == system | standard-world | <world name> |
;;;          ( (<predicate name> . ( (<args> . <body>) ... ) ) ... )

(DEFUN P@WITH (WORLD BODY $SUBST)
  (LET ((@URANUS-WORLD
	  (COND ((null world) (cons (gensym) @uranus-world))

		;;The system's world is NIL, which cannot be reached by (WITH NIL ...)
		;; It should be called via (WITH SYSTEM ...)
		;((eq world 'Uranus-User::system) nil) -- needed actually?

		;;The following special case STANDARD-WORLD should be handled by
		;; (WITHIN (STANDARD-WORLD) ... )
		;((eq world 'Uranus-User::standard-world) '(Uranus-User::standard-world nil))

		;; eliminate the dupulicated world and insert it into top posion.
		;;  "delete" destroys the structure, so "remove" is used instead.
		((ATOM WORLD) (CONS WORLD (remove world @URANUS-WORLD)))
		((listp world) (append world @uranus-world))
		(T (REPORT-ERROR "ILLEGAL ARGUMENT TO WITH" world))))
	(@level (1+ @level)))
    ;;;
    ;;; Create new environment to implment a world binding of a term description.
    ;;;                                     V
    (r@push-continuation body `(,(car $subst) . , @uranus-world))
    ))

(defun p@where (name preds subst)
  (p@where1 name @uranus-world preds subst))

(defun p@where1 (name $uranus-world preds $subst)
  (cond ((get name (car $uranus-world))
         (let* ((@clause (cdr preds))
		(@form (car preds))
		(@unseen-world @uranus-world)
		(@definitions (r@getdef @form $uranus-world))
		(@negations (pop @definitions))
		(@level (1+ @level))
		(@undo-point @undolist)
		(@final-undo-point @undolist)
		(@old-subst $subst)
		(@new-subst (newsubst))
		(@world-at-entrance @uranus-world))
           (or #|(catch :refute
                 (do ((@uranus-world $uranus-world))
                     (nil)
                   (refute@one)))|#
	       (let((@uranus-world $uranus-world))
		 (refute-one-loop))		;changed by tomura
               (and $uranus-world
                    (p@where1 name (cdr $uranus-world) preds $subst)))))
        ($uranus-world
         (p@where1 name (cdr $uranus-world) preds $subst))))

(defun p@within (world body $subst)
  (let ((@uranus-world (cond ((null world) nil)
			     ((symbolp world) @uranus-world)
			     (t world)))
	(@level (1+ @level)))
    (r@push-continuation body $subst)))


(DEFUN P@THROW ($FORM $SUBST)
  (throw :catch (cons $form $subst)))

(DEFUN P@LOOP (BODY $SUBST)
   (LET ((U @UNDOLIST))
    (DO NIL (NIL) (REFUTE@N BODY $SUBST (1+ @LEVEL)) (UNDO U))))

(DEFUN P@DO (BODY $subst)
  (do nil
      ((cond ((listp body))
             ((and (varp body) (assigned body $subst))
              (setq body (fetch body $subst))
              (setq $subst @fetched-subst)
              nil)
             (t (report-error "ILLEGAL FORM" body) t))))
  (do ((@clause nil nil)
       (@cue nil nil)
       (@stack nil nil)
       (@form (car body) (car body))
       (@definitions (r@getdef (car body) @uranus-world)
                     (r@getdef (car body) @uranus-world))
       (@undo-point @undolist @undolist)
       (@final-undo-point @undolist @undolist)
       (@old-subst $subst $subst)                       ;substitution fixed
       (@new-subst (newsubst) (newsubst))           ;1983/08/22
       ($level (1+ @level))
       (@level (1+ @level) $level)		;@level may be changed in refute-one-loop
       (@world-at-entrance @uranus-world))
      (nil)
    (setq @negations (pop @definitions))
    (refute-one-loop)
    (pop body)
    (cond ((null body) (return t))
          ((atom body) (return t)))))

(defun print@defs (flist world)
  (MAPC (FUNCTION
         (LAMBDA (NAME)
           (COND ((SYMBOLP NAME)
                  (COND ((get name world)
			 (p@list-neg-pos name (strip-last-nil (get name world))))
                        (T (PRINC "UNDEFINED PREDICATE :")
                           (PPRINT NAME) (TERPRI))))
                 (T (pprint NAME standard-output)))))
        FLIST))

(defun strip-last-nil (x)
  (cond ((null x) nil)
	((null (cdr x)) (if (null (car x)) nil x))
	(t (cons (car x) (strip-last-nil (cdr x))))))


(DEFUN P@NEXT (NAME @ARGS @SUBST)
   (CATCH :NEXT
           (P@NEXT1
	       NAME
	       @ARGS
	       (GET NAME :CONTEXT)
	       (GET NAME :COUNT)
	       (GET NAME :ORIGINAL-SUBST))))

(DEFUN P@NEXT1 (NAME ARGS CONTEXT COUNT ORIGINAL-SUBST)
   (COND (CONTEXT
            (LET* ((@FORM (POP CONTEXT)) (@CLAUSE (POP CONTEXT)) 
                  (@DEFINITIONS (POP CONTEXT))
		  (@negations (pop @definitions))
		  (@unseen-world (pop context))
		  (@OLD-SUBST (POP CONTEXT)) 
                  (@NEW-SUBST (POP CONTEXT))
		  ($UNDOLIST (POP CONTEXT)) 
                  (@UNDO-POINT NIL)
		  (@FINAL-UNDO-POINT NIL)
                  (@STACK (POP CONTEXT))
		  (@CUE (POP CONTEXT))
		  (@world-at-entrance @uranus-world))
	      (LET ((@UNDOLIST $UNDOLIST))
		(OR (ZEROP COUNT)
		    (CATCH :REFUTE (PROG1 T (R@FAIL)))
		    (progn
		      (setf (get name :context) nil)
		      (setf (get name :count) nil)
		      (setf (get name :original-subst) nil)
		      (THROW :NEXT NIL)))
		(COND ((refute-one-loop)
		       (setf (get NAME :context)
			     (LIST @FORM @CLAUSE @DEFINITIONS
				   @unseen-world
				   @OLD-SUBST @NEW-SUBST 
				   @UNDOLIST
				   @STACK @CUE))
		       (setf (get NAME :count) (1+ COUNT)))
		      (T (THROW :NEXT NIL))))
	      (UNIFY ARGS
		     @SUBST
		     (FETCHVALUE (GET NAME 'ARGS) ORIGINAL-SUBST)
		     ORIGINAL-SUBST)))
         (T (REPORT-ERROR "ILLEGAL ARGUMENT TO NEXT" NAME))))

(DEFUN P@INITIATE (FORM SUBST)
   (LET ((NAME (intern (symbol-name (GENSYM (symbol-name (CAR FORM)))))))
    (setf (get NAME :context)
       (LIST FORM NIL
             (R@GETDEF FORM @uranus-world) @unseen-world SUBST (NEWSUBST)
	     NIL NIL NIL))
    (setf (get NAME 'args) (CDR FORM))
    (setf (get NAME :count) 0.)
    (setf (get NAME :original-subst) SUBST)
    NAME))

(DEFUN RETRACT (Pattern P-SUBST body b-subst)
   (COND ((and (VARP Pattern)(ASSIGNED Pattern P-SUBST))
	  (RETRACT (CADR @FETCHED-VALUE) (CDDR @FETCHED-VALUE) body b-subst))
         ((symbolp pattern)
	  (let ((world (car @uranus-world)))
	    (if (get pattern world)
		(progn (remprop pattern world)
		       (p@retract-world world pattern)
		       t)
		nil)))
         ((listp pattern)
          (let ((p (fetchvalue pattern p-subst 2))
		(world @uranus-world))
            (do ((def (r@getdef p (car world)) (r@getdef p (car world))))
		((null world) nil)
	      (if
		(do ((d (cdr def) (cdr d))	;(car def) is NEGATIONS
		     (u @undolist)		;Variables in the head
						;must be undone, too.
		     (h-subst (newsubst '||)))
		    ((null d) nil)
		  (cond ((and (unify pattern p-subst (cons (car p) (caar d)) h-subst)
			      (unify body    b-subst (cdar d) h-subst))
			 (return (progn (or (setf (get (car p)
						     (car world))
						     (cons (car def)
							   (delq (car d) (cdr def))))
					    (p@retract-world (car world)
							     (car p))
					    )
					t)))
			(t (undo u))))
		(return t))
	      (pop world))))
         (T (REPORT-ERROR "ILLEGAL ARGUMENT TO RETRACT" Pattern))))

(DEFUN P@REGISTER-INTO-WORLD (WORLD PRED)
  ;; register predicate in the world
  (LET ((DEFS (GET WORLD :WORLD)))
    (or (MEMQ PRED DEFS) (setf (get WORLD :WORLD) (CONS PRED DEFS))))
  ;; register the world itself
  (or (memq world @world-list)
      (push world @world-list))
  ;; while reloading...
  (and @reload-flag
       (or (member (cons pred world) @reload-deflist :test #'equal)
	   (push (cons pred world) @reload-deflist)))
  t
  )

(defun p@retract-world (world pred)
       (setf (get world :world) (delq pred (get world :world))))


(DEFUN P@WORLD (NAME DEFS)
   (COND ((ATOM NAME)
          (LET ((NL NIL))
           (MAPC (FUNCTION
                  (LAMBDA (D)
                   (COND ((AND (LISTP D) (SYMBOLP (CAR D)))
                          (PUSH (CAR D) NL)
                          (setf (get (CAR D) NAME) (CDR D)))
                         (T (REPORT-ERROR "ILLEGAL ARGUMENT TO WORLD" D)))))
              DEFS)
           (setf (get NAME :WORLD) NL)))
         (T (REPORT-ERROR "ILLEGAL ARGUMENT TO WORLD" NAME))))

(DEFUN P@ERASE-WORLD (NAME)
   (MAPC (FUNCTION (LAMBDA (X) (REMPROP X NAME))) (GET NAME :WORLD))
   (setq @world-list (delq name @world-list))
   (REMPROP NAME :WORLD))

(defun p@backtrace (plevel)
  (mapcar (function (lambda (x)
                      (cons (car x)
                            (Trace-fetch-value (cadr x) (caddr x) plevel))))
          @backtrace))
