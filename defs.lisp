;;; -*- Mode:Lisp; Base:10.; Package: URA -*-
;;; This file is "defs.lisp".
;;; Uanus global viable definitions for Zetalisp
;;; July 4th, 1984
;;; (c) Hideyuki Nakashima and Satoru Tomura

;;; ------------------------------------------------------------
;;; Uranus update history

;;; March 1984     V-0.0
;;; June 1984      V-1.0  VARP speed up.  ATTENTION-HANDLER added.
;;; July 30 1984   V-1.1  WITH updated.
;;; July 31 1984   V-1.2  WITH backtracks.
;;; August 3 1984         local-cut and compiled-code is added.
;;; August 4 1984         LAMBDA and executalbe pattern updated.
;;;                V-2.0  CLAUSE added.
;;; August 6 1984  V-2.1  manipulation of @uranus-world fixed.
;;; August X 1984  V-2.2  Something is fixed.
;;; Sept. 27 1984  V-3.0  CLEAR-ALL added.
;;;                       LISP deleted.  QUIT, EPILOG changed.
;;;  --- Lisp Machine Release 5.2 ---
;;; Oct.  2  1984  V-4.0  subst implementation changed for gaea.(not completed)
;;; Oct. 18  1984  V-4.1  Interface to ZMACS added.
;;;                       ReLoad supported.
;;; Oct. 22  1984  V-4.2  Attention handler updated to allow normal continuation.
;;; Jan. 28  1985  V-4.3  Pretty printer called by PRINT is replaced by GRIND-TOP-LEVEL
;;;                       to support file output.
;;; Mar. 20  1985  V-4.4  Step-level updated.  Uranus window changed.
;;; Mar. 27  1985  V-4.5  @basic-world is changed by PUSH-WORLD.
;;;                       (to support PUSH-WORLD at the top-level)
;;; Mar. 29  1985  V-4.6  CLOSE fixed.
;;; Apr. 24  1985  V-5.0  DENY as negative information (test version).
;;; May  11  1985  V-5.1  EQ, CATCH, THROW updated.
;;; Jul. 24  1985  V-5.2  ASSERTQ added.
;;; Aug.  2  1985  V-6.0  Mouse facilities added.
;;; Sep.  5  1985  V-6.1  CATCH fixed.
;;; Oct. 22  1985  V-6.2  (WITH STANDARD-WORLD/SYSTEM ...) aren't treated specially any more.
;;; Oct. 29  1985  V-6.3  ERASE-WORLD removes the world from @world-list
;;;                       UNIVERSE added
;;; Nov. 26  1985  V-6.4  COPY added
;;; Feb.  5  1986  V-6.5  WHERE changed. by Tomura
;;; Feb.  9  1986  V-7.0  CLAUSE added
;;;                       To support for CLAUSE to backtrack,  "def-sys-multi-pred" added.
;;; Mar. 17  1986  V-8.0  Test for negative info. moved to the end of the clause.
;;; Mar. 26  1986  V-9.0  REFUTE@ONE macro --> defun,  CLAUSE fixed.
;;; Apr.  1  1986 V-10.0  TD introduced.
;;; Apr.  4  1986 V-10.8  (1) TD remembers its world context.
;;;                       (2) Eager TD added.
;;;                       (3) =Q= (unify-q-q) added.  CLAUSE doesn't execute TD's.
;;; Apr. 17  1986 V-11.0  WITH does not take definitions any more.
;;;                       TD extended: [term ! (pred)], [world||pred], [term ! (world||pred)].
;;; Jun.  2  1986 V-12.0  OBJECTIZE and METIZE supported.
;;;              1986 V-13.0  ?
;;; Sep.  9  1986 V-14.0  @saved-vars added for local variables for backtrackable system predicates.
;;; Mar. 20  1987 V-15.0  The scope of negative information fixed according to the semantics.
;;;                       Negative information is searched only for inner worlds of
;;;                       the positive clause, not the whole nesting.
;;; Sept. 2  1987 V-17.0  Whole system has been transported into Common Lisp. 
;;;
;;; Jan.  28 1988 V-18.0  Maintained by S. Tomura
;;;                       New predicates:
;;;                         metap *term : succees iff *term is a metized term.
;;;                         meta-level *term *level : *level is a metized level of *term.
;;;                       Recording facility has been fixed. (#+symbolics)
;;;                       Attention handler was installed. (#+vaxlisp)
;;; Jul.   1 1988 V-18.1  Modified by S. Tomura
;;;                       Attention hanlder was installed. (#+kcl)
;;; Sep.   9 1988 V-19    "UNDEFINED PREDICATE" error issued in (DEBUG ON)
;;;                       only.
;;;                       Copy does not execute TD (fetchvalue -> fetchvalue-q).
;;; -----------------------------------------------------------

(in-package :uranus)


(defvar uranus@version "V-19.8")


(eval-when (:compile-toplevel :load-toplevel :execute)


;;; The things to be fixed:
;;;
;;; To be done:  freeze and  melt , or one-directional unification(subsumption)
;;;           :  debugging mechanism

;(export 'initial-readtable 'system)

(defvar uranus-readtable (copy-readtable nil) "readtable for uranus system")


;;; Internal registers of the Uranus kernel machine.
(defvar @LAST-RESULT nil "")

(defvar @reload-flag nil "T if reloading a file")
(defvar @reload-deflist nil "List of reloading predicate names")


(defvar @DEBUG nil "Debug flag = @step or @trace")
(defvar @DEBUG1 nil "")

(defvar @STEP nil "Flag for STEP")
(defvar @error-or-attention nil "Flag for ERROR")
(defvar @attention nil "Flag for ATTENTION")
(defvar @SELECTSTEP nil "")
(defvar @STEPLEVEL 99999.  "Max depth of stepper")

(defvar @TRACE nil "List of traced predicates")
(defvar @TRACEALL nil "")

(defvar @PRINTLEVEL 20. "Level of usual printing")


#+symbolics
(defvar @DEFAULT-PATHNAME (fs:make-pathname-defaults) "Default pathname for users")

#-symbolics
(defvar @DEFAULT-PATHNAME "init.ura" "")

(defvar @FORM nil "")
(defvar @LEVEL 0. "")
(defvar @CUE nil "")
(defvar @STACK nil "")
(defvar @father nil "Backtrack point for cut")
(defvar @mother nil "Backtrack point for local cut")
(defvar @GOAL nil "")
(defvar @CLAUSE nil "")
(defvar @unseen-world nil "List of outer unseen worlds")	;added 1985-04-24
(defvar @DEFINITIONS nil "Pointer to yet untried assertions")
(defvar @negations nil "Pointer to yet untried negations")	;added 1985-04-24
(defvar @saved-vars nil "(Local-variables . values) for backtrackable system predicates")	;added 1986-09-09
(defvar @UNDOLIST nil "")
(defvar @UNDO-POINT nil "")
(defvar @OLD-SUBST nil "")
(defvar @NEW-SUBST nil "")
(defvar @FINAL-UNDO-POINT nil "")
(defvar @FETCHED-VALUE nil "")
(defvar @FETCHED-SUBST nil "")

(defvar @URANUS-WORLD nil "")
(defvar @basic-world nil "")
(defvar @world-at-entrance nil "")
(defvar @world-list '(Uranus-User::standard-world) "List of all worlds")

(defvar @BACKTRACE nil "")

(defvar @ARGS nil "")
(defvar @SUBST nil "")
(defvar @BIND nil "")

(defvar *subst-no* 0 "substitution no.")

(defvar terminal-input nil)
(defvar terminal-output nil)
(defvar standard-output)
(defvar standard-input)

(defvar line-length nil "")
(defvar attention-handler nil "")

(defvar *uranus-listener-window* nil "Uranus listener window")


;;(defvar amuse:amuse-view-window nil "This window display the view of buffer")
;;(defvar amuse:amuse-print-window nil "This window display the print-image of buffer")
;;(defvar amuse:amuse-command-window nil "This window is a command input window")

)
