(in-package :uranus)

(let ((rt uranus-readtable))
  (set-macro-character #\[ #'uranus-read-\[ nil rt)
  #|(set-macro-character       #\] #'(lambda (x y) '\]))|#
  (set-macro-character #\] (get-macro-character #\) nil) nil rt)
  (set-macro-character #\{ #'uranus-read-\{ nil rt)
  #|(set-macro-character       #\} #'(lambda (x y) '\}))|#
  (set-macro-character #\} (get-macro-character #\) nil) nil rt)
  (set-macro-character #\@ #'uranus-read-\@ nil rt))

(let ((*readtable* uranus-readtable))
  ;(read-from-string "@(cons 1 2)")
  (read-from-string "[p 1]")
  )


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
