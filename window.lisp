;;; -*- Mode: Lisp; Base: 10.; Syntax: Common-lisp; Package: URA; -*-
;;; Copyright 1987, 1985 Hideyuki Nakashima

(in-package 'uranus)


(zl:defflavor Uranus-listener ()
     ( ; tv:mouse-sensitive-text-scroll-window
       ; tv:basic-scroll-bar
       tv:window tv:process-mixin tv:stream-mixin
      tv:window-with-typeout-mixin)
     (:default-init-plist
      :TYPEOUT-WINDOW '(TV:TEMPORARY-TYPEOUT-WINDOW)
      :SAVE-BITS T
      :activate-p t
      :expose-p t
      :name "Uranus listener"
;      :font-map '(fonts:medfnt fonts:cptfont fonts:medfnb)
      :process
      '(uranus-initial-function :regular-pdl-size #8r40000 :special-pdl-size #8r4000)
      ;;; no :edges-from option means default window's size (i.e. full screen).
      ))
     

(defun uranus-initial-function (*terminal-io*)
  (let ((terminal-input *terminal-io*)
	(terminal-output *terminal-io*)
	(standard-input *terminal-io*)
	(standard-output *terminal-io*))
    (init)
    (zl:error-restart-loop (sys:abort "[Abort all!!]")  ;;; Who catches the error?
      (setq uranus@version 
	    (multiple-value-bind (majar minar status)
		(sct:get-system-version 'uranus)
	      (concatenate 'string 
			   (format nil "V-~d.~d " majar minar)
			   (if (eq status :released) ""
				 (string status)))))
      (zl:send *terminal-io* :set-label
	       `(:bottom
		  :font fonts:uranus
		  :string 
		  ,(format nil "  system ~a" uranus@version)))
      (pgo))))


(tv:add-select-key #\U 'uranus-listener "Uranus system" t)

(tv:add-to-system-menu-programs-column
  "Uranus system"
  '(tv:select-or-create-window-of-flavor 'uranus-listener)
  "Uranus : Universal Representation-Aimed Novel Uranus System"
  t
)

(tv:add-to-system-menu-create-menu
  "Uranus system"
  'uranus-listener
  "Uranus : Universal Representation-Aimed Novel Uranus System"
  t)


;;; To invoke Uranus system, use "select" facility. (ex. select key, system mouse menu)


#|
;;; You cannot do any actual work in the following method.
;;; The following program does not run!
;;; Prepare to halt machine if you try.

(zl:defmethod (:mouse-click uranus-listener) (click x y)
  (let ((op (case click
	      (#\mouse-L-1 (input-assertion))
	      (#\mouse-L-2 (load-save-or-display-definitions))
	      (#\mouse-M-1 (set-print-level))
	      (#\mouse-M-2 (stepper-on-off))
	      (#\mouse-R-1 (menu-of-other-operations)))))
    (ZL:SEND self :FORCE-KBD-INPUT
	  `(:MOUSE-BUTTON ,click ,self ,x ,Y))
t
))
|#


(defun read-from-window (prompt)
  (in-package :cl-user)
  (terpri) (princ prompt)
  (or
    (let ((blip (zl:send *terminal-io* ':any-tyi)))
      (cond ((listp blip)
	     (case
	       (first blip)
	       (:mouse-button
		(let ((click (second blip)))
		(case click
		  (#\mouse-L (input-assertion))
		  (#\sh-mouse-L (load-save-or-display-definitions))
		  (#\mouse-M (set-print-level))
		  (#\sh-mouse-M (stepper-on-off))
		  (#\mouse-R (menu-of-other-operations)))))
	       (otherwise (zl:beep))))
	    ((char-equal blip #\HELP)
	     (send-help-to *terminal-io*)
	     nil)
	    ((char-equal blip #\REFRESH)
	     (zl:send *terminal-io* ':clear-window))
	    (t
	     (zl:send *terminal-io* ':untyi blip)
	     (read))))
    (throw :uranusloop nil)))

(zl:defmethod (:who-line-documentation-string uranus-listener) ()
  "L: Assertq,  L2: Load//Save//Display Defs,  M: Set Print Level,  M2: Stepper On//Off,  R: Menu of Other Operations, R2: System Menu.  ")

(defvar *definitions-item-list*
	'(("Load    File"
	   :funcall load-file
	   :documentation "Load a File")
	  ("Re-load   File"
	   :funcall reload-file
	   :documentation "Load a File and Replace Old Assertions with New")
	  (" Save   Definitions "
	   :funcall save-predicate-definitions
	   :documentation "Save Predicate Definitions in a File")
	  (" Display Definitions "
	   :funcall display-predicate-definitions
	   :documentation "Display Predicate Definitions on the Terminal")))

(defun load-save-or-display-definitions ()
  (multiple-value-bind (result err)
    (catch 'errset
      (tv:menu-choose
	*definitions-item-list*
	"Select Operation"))
    (if err result nil)))

(defvar *defs-file* "init.ura")
(defun load-file ()
  (catch 'load-file
    (let ((fs:*default-pathname-defaults*
	    (fs:merge-pathnames (fs:user-homedir) @default-pathname)))
      (tv:choose-variable-values
	'((*defs-file* "File to Load" :pathname))
	:label nil
	:margin-choices '("Load" ("Abort" (throw 'load-file nil)))))
    (load-file-selected)))

(defun load-file-selected ()
  (format t "Loading from ~a ..." *defs-file*)
  (p@load (list *defs-file*))
  (format t " Loaded.~%")
  )

(defun reload-file ()
  (let ((@reload-flag t)
	(@reload-deflist nil))
    (load-file)))


(defun save-predicate-definitions ()
 (case (tv:menu-choose
	   '((" Select Worlds to Save " :value world)
	     (" Save  All  Worlds "         :value all-world)
	     (" Select Predicates to Save " :value predicate)))
   (world (let ((world-to-be-saved
		  (get-some-symbols-from
		    (sort @world-list #'string-lessp)
		    "Select World to Save"
		    "SAVE")))
	    (if world-to-be-saved
		(mapcar #'(lambda (w)
			    (format t "Dumping to a file ~a.ura ..." w)
			    (p@dump (string w) (get w :world))
			    (format t " Dumped.~%"))
			world-to-be-saved))))
   (all (mapcar #'(lambda (w)
		    (format t "Dumping to a file ~a.ura ..." w)
		    (p@dump (string w) (get w :world))
		    (format t " Dumped.~%"))
		@world-list))
   (predicate (let ((symbols-to-be-saved
		      (get-some-symbols-from
			(sort (get (car @uranus-world) :world) 'alphalessp)
			(concatenate 'string "Select Predicates To Save in World "
				       (car @uranus-world))
			"SAVE")))
		(if symbols-to-be-saved
		    (progn (format t "Writing to a file ~a.ura ..." (car @uranus-world))
			   (p@dump (string (car @uranus-world)) symbols-to-be-saved)
			   (format t " Done.~%")))))))

(defun display-predicate-definitions ()
  (let ((predicate-to-be-shown
	  (get-some-symbols-from
	    (get (car @uranus-world) :world)
	    (concatenate 'string  "Select Predicates To List in World " (car @uranus-world))
	    "LISTING")))
    (p@listing predicate-to-be-shown (newsubst))
    nil))

(defun get-some-symbols-from (symbols label prompt)
  (apply #'append
	 (mapcar #'(lambda (list) (and (second list) (list (first list))))
		 (tv:multiple-choose
		   label
		   ;; presently defined symbols
		   (mapcar
		     #'(lambda (sym) (list sym (string sym) '(t)))
		     symbols)
		   `((t ,prompt))))))

(defun set-print-level ()
  (tv:choose-variable-values
    '((@printlevel "Print Level (0 for infinity)" :number))
    :label nil)
  nil)

(defun stepper-on-off ()
  (cond (@step
	 (if (tv:menu-choose
	       '((" Turn Off Stepping (Currently ON) " :value t)))
	     (make-step-off)))
	(t
	 (if (tv:menu-choose
	       '((" Turn On Stepping (Currently OFF) " :value t)))
	     (make-step-on))))
  nil)

(defun input-assertion ()
  (zl:with-input-editing-options ((:initial-input "(assertq ())" nil nil 10))
    (read)))

(defvar *other-operations-item-list*
	`(("HELP" :eval (send-help-to *terminal-io*)
	   :documentation "Get some help on LNF.")
	  ("Trace" :eval (start-or-cont-tracing)
	   :documentation
	   "Start tracing or add symbols to list of symbols currently being traced.")
	  ("UnTrace" :eval (end-or-cont-tracing)
	   :documentation "End tracing or remove some of the symbols being traced.")
	  ("Start  Session  Recording" :eval (start-recording)
	   :documentation "Route a copy of the session to a file.")
	  ("End    Session  Recording" :eval (end-recording)
	   :documentation "Quit recording this session.")
	  ("Turn On Garbage Collector" :eval (gc-on)
	   :documentation "Start the ZetaLisp Ephemeral Garbage Collector")
	  (" Turn Off Garbage Collector " :eval (gc-off)
	   :documentation "Stop the ZetaLisp Ephemeral Garbage Collector")))

(defvar *documentation*
	"
You are typing to Uranus system.

The top level loop expects a predicate-call to be typed in.
Here are examples:
  To define a predicate:  (assert (human *x) (man *x))
  To execute a preidcate: (append (1 2) (3 4) *x)
        Or alternatively: [append (1 2) (3 4)]

The print-out is controlled by a predicate PRINT-LEVEL.
You may set the level by (PRINT-LEVEL 99).
The default is 7.


")

(defun send-help-to (window)
  (tv:with-terminal-io-on-typeout-window (window t)
      (format t *documentation*)))

(defun start-or-cont-tracing ()
  (ask-for-symbols-to-be-traced) nil)

(defun ask-for-symbols-to-be-traced ()
  (let ((symbols-to-be-added-to-trace
	  (get-some-symbols-from
	    (let ((l (get (car @uranus-world) :world)))
	      (mapc #'(lambda (x) (setq l (delq x l)))
		    @trace)
	      l)
	    (concatenate 'string  "Mark Predicates to Trace in World " (car @uranus-world))
	    "TRACE")))
	(setq @trace (append symbols-to-be-added-to-trace @trace)))
  nil)

(defun ask-which-symbols-are-to-be-removed ()
  (let ((symbol
	  (get-some-symbols-from
	    @trace
	    "Select Predicates not to Trace"
	    "UNTRACE")))
    (mapc #'(lambda (x) (setq @trace (delq x @trace)))
	  symbol))
  nil)

(defun end-or-cont-tracing ()
  (cond (@traceall
	 (cond ((tv:menu-choose
		  '(("End Tracing" :value t)))
		;wants to quit tracing altogether
		(setq @traceall nil))))
	(@trace
	 (ask-which-symbols-are-to-be-removed))
	(t (case (tv:menu-choose
		     '(("Trace All Predicates" :value all)
		       ("Trace Selectively" :value select))
		     "No tracing is going on")
	     (all (setq @traceall t))
	     (select (ask-for-symbols-to-be-traced)))))
  nil)

(defun menu-of-other-operations ()
  (tv:menu-choose
    *other-operations-item-list*
    "Make Selection")
  nil)

(defvar *recording-file*  "uranus.record")
(defvar *recording* nil)

(defvar *uranus-dribble-stream* nil)


;;; CAUTION::
;;; The codes of "start-recording" and "end-recording" was made by modifying 
;;; zl:dribble-start and zl:dribble-end.
;;;

(defun start-recording ()
  (cond ((si:inside-dribble?)
	 (format t "~&Session is alreadly being recorded in ~a" *recording-file*)
	 )
	(t (let ((fs:*default-pathname-defaults*  zwei:*pathname-defaults*))
	     (tv:choose-variable-values
	       '((*recording-file* "File in which to record session" :pathname))
	       :label nil))
	   (let ((pathname
		   (fs:merge-pathnames *recording-file* (fs:default-pathname nil nil :text))))
	     (with-open-stream
		 (dribble-stream
		   (si:make-instance 'si:dribble-stream
				  :tv-stream *terminal-io*
				  :file-stream
				  (open pathname :direction :output 
					:if-does-not-exist :create
					:if-exists :new-version
					:error :reprompt)
				  ))
		 (format t "~&[ Uranus session is now being recorded in ~a. ]~2%" *recording-file*)
		 (setf *uranus-dribble-stream* dribble-stream)
		 (let ((standard-input  dribble-stream)
		       (standard-output dribble-stream)
		       (terminal-input  dribble-stream)
		       (terminal-output dribble-stream)
		       (si:standard-input  dribble-stream)
		       (si:standard-output dribble-stream)
		       (si:error-output    dribble-stream)
		       (si:query-io        dribble-stream)
		       (si:trace-output    dribble-stream)
		       )
		   (format t "~&;;; Recorded from Uranus session on:")
		   (format t "~&;;; Start of Recording at: ~\\date\\~%" (time:get-universal-time))
		   (catch 'dribble-end
		     ;; Do what LISP-COMMAND-LOOP does, but without re-binding streams to syn streams
		     ;; terminal-io and debug-io remain attached to the terminal, the others get filed
		     (pgo))))
	       (format t "~&~2%[ Session is no longer being recorded. ]~%"))))
  t)

(defun end-recording ()
  (cond ((not (si:inside-dribble?))
	 (format t "~&Output is not currently being recorded.~%")
	 nil)
	(t 
	 (format t "~%;;; End of Recording at: ~\\date\\~%" (time:get-universal-time))
	 (throw 'dribble-end (close *uranus-dribble-stream*)))))


