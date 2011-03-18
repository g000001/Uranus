;;; -*- Mode: LISP; Package: ZWEI; Base: 8; Syntax: Common-lisp -*-
;;;
;;; Zwei mode definition for Uranus

(DEFFLAVOR URANUS-LANGUAGE-MIXIN () (LISP-SYNTAX-MIXIN))

(DEFMETHOD (:EVAL-PRINT-FUNCTION URANUS-LANGUAGE-MIXIN) ()
  #'URANUS:toplevel-EXECUTE)

(DEFCOM COM-ReEVALUATE-AND-EXIT "ReEvaluates the buffer and return from top-level" ()
  (LET ((URANUS:@RELOAD-DEFLIST NIL)
	(URANUS:@RELOAD-FLAG T))
    uranus:@reload-deflist
    uranus:@reload-flag
    (COM-EVALUATE-Uranus-AND-EXIT)))

(defun com-evaluate-uranus-and-exit ()
  (let ((readtable uranus:uranus-readtable)	;for Rel-5
	(si:standard-readtable uranus:uranus-readtable)	;for Rel-6
	(scl:*readtable* uranus:uranus-readtable)	;for Rel-7
	(uranus:@printlevel 5))
    (com-evaluate-and-exit)))

(defun com-evaluate-uranus-region ()
  (let ((readtable uranus:uranus-readtable)	;for Rel-5
	(si:standard-readtable uranus:uranus-readtable)	;for Rel-6
	(scl:*readtable* uranus:uranus-readtable)	;for Rel-7
	(uranus:@printlevel 99))
    (com-evaluate-region)))

(DEFMETHOD (:COMPILATION-SUPPORTED URANUS-LANGUAGE-MIXIN) () NIL)

(DEFMETHOD (:DEFAULT-SOURCE-FILE-TYPE URANUS-LANGUAGE-MIXIN) () :URA)

(DEFFLAVOR URANUS-MODE ()
	   (URANUS-LANGUAGE-MIXIN
	    LISP-SYNTAX-MODE-FORMS-MIXIN LISP-LANGUAGE-MIXIN MAJOR-MODE))

(DEFMETHOD (:MODE-FORMS URANUS-MODE) ()
  `((SET-COMTAB *MODE-COMTAB* '(#\m-Z COM-EVALUATE-Uranus-AND-EXIT
				#\c-m-Z COM-REEVALUATE-AND-EXIT
				#\c-sh-E com-evaluate-Uranus-region
		))))

(DEFMODE COM-URANUS-MODE URANUS-MODE "Sets things up for editing Uranus code."
  :URANUS)

(SET-COMTAB *ZMACS-COMTAB*
	    ()
	    (MAKE-COMMAND-ALIST '(COM-URANUS-MODE)))

(push '(:ura    . :uranus) fs:*file-type-mode-alist*)
(push '(:uranus . :uranus) fs:*file-type-mode-alist*)

(fs:define-canonical-type :uranus "URA")
