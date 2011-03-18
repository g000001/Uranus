;;; -*- Mode: Lisp; Package: Utilisp; -*-
;;; Print with Indentation system
;;; (c) T. Chikayama

(IN-PACKAGE :URANUS-USER)

(defvar line-length 80. "")
(defvar colleft nil "")
(defvar plen nil "")
(DEFVAR *CURSOR-POS* 0)

;(setq line-length 80.)
(DEFUN CURSOR ()
  (MOD *CURSOR-POS* LINE-LENGTH))

(DEFUN TAB (N)
  (LOOP :REPEAT N :DO (PRINC "  "))
  (INCF *CURSOR-POS* (* N 2)))
(DEFUN MEMQ (FN LIST)
  (MEMBER FN LIST :TEST #'EQ))
(DEFUN ASSQ (ITEM ALIST)
  (ASSOC ITEM ALIST :TEST #'EQ))
(DEFUN FLATSIZE (X)
  (LENGTH (PRINC-TO-STRING X)))

(DEFUN PRINCC (OBJ)
  (PRINC OBJ)
  (INCF *CURSOR-POS* (LENGTH (PRINC-TO-STRING OBJ))))

(DEFUN PRIN1C (OBJ)
  (PRIN1 OBJ)
  (INCF *CURSOR-POS* (LENGTH (PRIN1-TO-STRING OBJ))))

(defvar usebq nil "use back quote sytle")

#+maclisp
(DECLARE (*LEXPR prind @PRIND))

(DEFUN PRIND (ITEM &OPTIONAL (ASBLOCK) (PLEV) (PLEN))
  (SETQ *CURSOR-POS* 0)
   (AND PLEV
        (SETQ ITEM (@PRUNE ITEM PLEV (COND (PLEN (1- PLEN)) (T 999999.)))))
   (AND USEBQ (NOT PLEV) (SETQ ITEM (ELIMINATE-COMMA (USEBQ ITEM))))
;   (TERPRI)
   (@PRIND ITEM (CURSOR) 0. ASBLOCK))

(DEFUN @PRUNE (ITEM PLEV PL)
   (COND ((ATOM ITEM) ITEM)
         ((zerop PLEV) 'URANUS-User:?)
         ((AND (zerop PL) (CDR ITEM)) '(URANUS-User:???))
         (T (CONS (@PRUNE (CAR ITEM) (1- PLEV) PLEN)
                  (@PRUNE (CDR ITEM) PLEV (1- PL))))))

(DEFUN @PRIND (ITEM INDENT &OPTIONAL (CLOSE 0) (ASBLOCK))
   (TAB INDENT)
   (COND ((ATOM ITEM) (PRIN1C ITEM))
         ((AND (MEMQ (CAR ITEM) '(QUOTE BQ COMMA))
               (LISTP (CDR ITEM))
               (NULL (CDDR ITEM)))
          (@QUOTE ITEM INDENT CLOSE))
         ((OR (PRINTABLE ITEM (- line-length (CURSOR) CLOSE))
              (AND (ATOM (CAR ITEM)) (ATOM (CDR ITEM))))
          (@PRIN1 ITEM))
         ((EQ (CAR ITEM) 'DOTTED) (@DOTTED ITEM INDENT CLOSE))
         (T (PRINCC "(")
            (FUNCALL (COND ((OR ASBLOCK (< (- line-length (CURSOR) CLOSE) 40.))
                            (FUNCTION @BLOCK))
                           ((SYMBOLP (CAR ITEM))
                            (OR (AND (EVERY #'(lambda (x) (cond ((atom x) nil)
								((atom (cdr x)) nil)
								(t (cdr x))))
                                            ;(FUNCTION ATOM) 
                                            (CDR ITEM))
						;This ste-function is a patch.
						;EVERY does not handle a dotted pair
						;at the end of a list.
                                     (FUNCTION @BLOCK))
                                (GET (CAR ITEM) 'PRIND)
                                (FUNCTION @STANDARD)))
                           ((OR (ATOM (CAR ITEM))
                                (AND (CDAR ITEM) (ATOM (CDAR ITEM))))
                            (FUNCTION @BLOCK))
                           (T (FUNCTION @MISER)))
               ITEM
               (1+ INDENT)
               (1+ CLOSE))
            (PRINCC ")")(VALUES))))

(DEFUN @PRIN1 (ITEM)
   (COND ((ATOM ITEM) (PRIN1 ITEM))
         ((AND (MEMQ (CAR ITEM) '(QUOTE BQ COMMA))
               (LISTP (CDR ITEM))
               (NULL (CDDR ITEM)))
          (PRINCC (CDR (ASSQ (CAR ITEM)
                         '((QUOTE . \') (BQ . \`) (COMMA . \,))))
                )
          (@PRIN1 (CADR ITEM)))
         ((EQ (CAR ITEM) 'DOTTED)
          (PRINCC "(")
          (DO NIL
              (NIL)
              (@PRIN1 (FIRST (SECOND ITEM)))
              (OR (AND (LISTP (SECOND (SECOND ITEM)))
                       (EQ (CAR (SECOND (SECOND ITEM))) 'DOTTED))
                  (RETURN NIL))
              (PRINCC " ")
              (SETQ ITEM (SECOND (SECOND ITEM))))
          (AND (NOT (NULL (SECOND (SECOND ITEM))))
               (PROGN (PRINCC " . ")
                      (@PRIN1 (SECOND (SECOND ITEM)))))
          (PRINCC ")")(VALUES))
         (T (PRINCC "(")
            (DO NIL
                (NIL)
                (@PRIN1 (POP ITEM))
                (COND ((NULL ITEM) (RETURN NIL))
                      ((LISTP ITEM) (PRINCC " "))
                      (T (PRINCC " . ")
                         (PRIN1 ITEM)
                         (RETURN NIL))))
            (PRINCC ")")(VALUES))))


(DEFUN @STANDARD (ITEM INDENT CLOSE)
   (PRIN1C (CAR ITEM))
   (@MISER (CDR ITEM)
           (LET ((N (FLATSIZE (CAR ITEM))))
            (+ INDENT 1. (COND ((> N 6.) 1.) (T N))))
           CLOSE))

(DEFUN @QUOTE (ITEM INDENT CLOSE)
   (PRINCC (CDR (ASSQ (CAR ITEM) '((QUOTE . \') (BQ . \`) (COMMA . \,))))
         )
   (@PRIND (CADR ITEM) (1+ INDENT) CLOSE))

(DEFUN @DOTTED (ITEM INDENT CLOSE)
   (PRINCC "(")
   (DO NIL
       (NIL)
       (@PRIND (CAR (SECOND ITEM)) (1+ INDENT) CLOSE)
       (OR (AND (LISTP (SECOND (SECOND ITEM)))
                (EQ (CAR (SECOND (SECOND ITEM))) 'DOTTED))
           (RETURN NIL))
       (PRINCC " ")
       (SETQ ITEM (SECOND (SECOND ITEM))))
   (AND (NOT (NULL (SECOND (SECOND ITEM))))
        (PROGN (COND ((> (- LINE-LENGTH (CURSOR)) 3.)
                      (PRINCC " . ")
                      (TAB (1+ INDENT)))
                     (T (TAB INDENT) (PRINCC " . ")))
               (@PRIND (SECOND (SECOND ITEM)) (CURSOR) (1+ CLOSE))))
   (PRINCC ")")(VALUES))

(DEFUN @MISER (ITEM INDENT CLOSE)
   (DO NIL
       ((ATOM ITEM))
       (@PRIND (POP ITEM) INDENT (COND ((NULL ITEM) CLOSE) (T 0.))))
   (OR (NULL ITEM)
       (PROGN (TAB INDENT)
              (PRINCC ". ")
              (PRIN1C ITEM))))

(DEFUN @SETQ (ITEM INDENT CLOSE)
   (PRIN1C (POP ITEM))
   (SETQ INDENT (1+ (CURSOR)))
   (DO NIL
       ((OR (ATOM ITEM) (NOT (SYMBOLP (CAR ITEM))) (ATOM (CDR ITEM))))
       (@PRIND (POP ITEM) INDENT 0.)
       (PRINCC " ")
       (@PRIND (CAR ITEM)
               (COND ((PRINTABLE
                         (POP ITEM)
                         (- LINE-LENGTH (CURSOR) (COND ((NULL ITEM) CLOSE) (T 1.))))
                      (CURSOR))
                     (T INDENT))
               (COND ((NULL ITEM) CLOSE) (T 0.))))
   (@MISER ITEM INDENT CLOSE))

(DEFUN @DEFUN (ITEM INDENT CLOSE)
   (COND ((LISTP (SECOND ITEM)) (@BLOCK ITEM INDENT CLOSE))
         (T (PRIN1C (POP ITEM))
            (PRINCC " ")
            (@WITHVARS ITEM INDENT CLOSE))))

(DEFUN @WITHVARS (ITEM INDENT CLOSE)
   (PRIN1C (POP ITEM))
   (PRINCC " ")
   (@PRIND (POP ITEM) (CURSOR) 0. T)
   (@MISER ITEM (+ INDENT 2.) CLOSE))

(DEFUN @LAMBDA (ITEM INDENT CLOSE)
   (PRIN1C (POP ITEM))
   (PRINCC " ")
   (@PRIND (POP ITEM) (CURSOR) 0. T)
   (@MISER ITEM INDENT CLOSE))

(DEFUN @FUNCTION (ITEM INDENT CLOSE)
   (PRIN1C (POP ITEM))
   (@MISER ITEM INDENT CLOSE))

(DEFUN @FUNCALL (ITEM INDENT CLOSE)
   (PRIN1C (POP ITEM))
   (PRINCC " ")
   (@PRIND (POP ITEM) (CURSOR) 0.)
   (@MISER ITEM (+ INDENT 2.) CLOSE))

(DEFUN @PROG (ITEM INDENT CLOSE)
   (PRIN1C (POP ITEM))
   (PRINCC " ")
   (@PRIND (POP ITEM) (CURSOR) 0. T)
   (SETQ INDENT (+ INDENT 2.))
   (DO NIL
       (NIL)
       (AND (ATOM ITEM) (RETURN NIL))
       (@PRIND (CAR ITEM)
               (COND ((ATOM (POP ITEM)) (MAX 1. (- INDENT 10.))) (T INDENT))
               (COND ((NULL ITEM) CLOSE) (T 0.))))
   (OR (NULL ITEM)
       (PROGN (TAB INDENT)
              (PRINCC ". ")
              (PRIN1C ITEM))))

(DEFUN @BLOCK (ITEM INDENT CLOSE)
   (DO NIL
       ((ATOM ITEM)
        (OR (NULL ITEM)
            (PROGN (OR (> (CURSOR) INDENT) (TAB INDENT))
                   (OR (PRINTABLE
                          ITEM
                          (- (MIN LINE-LENGTH (+ INDENT 80.)) (CURSOR) CLOSE 2.))
                       (TAB INDENT))
                   (PRINCC " . ")
                   (PRIN1C ITEM))))
       (COND ((OR (PRINTABLE
                     (CAR ITEM)
                     (- (MIN LINE-LENGTH (+ INDENT 80.))
                        (CURSOR)
                        (COND ((NULL (CDR ITEM)) CLOSE) (T 1.))))
                  (AND (> (CURSOR) INDENT)
                       (PROGN (TAB INDENT)
                              (PRINTABLE
                                 (CAR ITEM)
                                 (- (MIN LINE-LENGTH (+ INDENT 80.))
                                    INDENT
                                    (COND ((NULL (CDR ITEM)) CLOSE) 
                                     (T 1.)))))))
              (@PRIN1 (POP ITEM))
              (OR (ATOM ITEM) (EQ (CURSOR) LINE-LENGTH) (PRINCC " ")))
             (T (@PRIND (POP ITEM) (CURSOR) (COND ((NULL ITEM) CLOSE) (T 0.)))
                (OR (NULL ITEM) (TAB INDENT))))))

(DEFUN PRINTABLE (ITEM COLLEFT) (@PRINTABLE ITEM))

(DEFUN @PRINTABLE (ITEM)
   (COND ((ATOM ITEM) (< 0 (SETQ COLLEFT (- COLLEFT (flatsize ITEM)))))
         ((AND (MEMQ (CAR ITEM) '(COMMA BQ QUOTE))
               (LISTP (CDR ITEM))
               (NULL (CDDR ITEM)))
          (SETQ COLLEFT (- COLLEFT 1.))
          (@PRINTABLE (CADR ITEM)))
         ((EQ (CAR ITEM) 'DOTTED)
          (SETQ COLLEFT (- COLLEFT 2.))
          (DO NIL
              (NIL)
              (OR (@PRINTABLE (FIRST (SECOND ITEM))) (RETURN NIL))
              (OR (AND (LISTP (SECOND (SECOND ITEM)))
                       (EQ (CAR (SECOND (SECOND ITEM))) 'DOTTED))
                  (RETURN (COND ((NULL (SECOND (SECOND ITEM))) (< 0 COLLEFT))
                                (T (SETQ COLLEFT (- COLLEFT 3.))
                                   (@PRINTABLE (SECOND (SECOND ITEM)))))))
              (SETQ ITEM (SECOND (SECOND ITEM)))
              (SETQ COLLEFT (1- COLLEFT))))
         (T (DO NIL
                (NIL)
                (AND (ATOM ITEM)
                     (RETURN (< 0 (SETQ COLLEFT 
                                       (- COLLEFT 
                                        (COND ((NULL ITEM) 1.) 
                                         (T (+ (FLATSIZE ITEM) 5.))))))))
                (SETQ COLLEFT (1- COLLEFT))
                (OR (@PRINTABLE (CAR ITEM)) (RETURN NIL))
                (POP ITEM)))))

(DEFUN USEBQ (ITEM)
   (COND ((ATOM ITEM) ITEM)
         ((CDR (LAST ITEM)) ITEM)
         ((MEMQ (CAR ITEM) '(BQ COMMA DOTTED))
          (LIST (CAR ITEM) (USEBQ (SECOND ITEM))))
         (T (LET ((Y (MAPCAR (FUNCTION USEBQ) ITEM)))
             (COND ((AND (EQ (CAR Y) 'CONS)
                         (LISTP (CDR Y))
                         (LISTP (CDDR Y))
                         (NULL (CDDDR Y))
                         (OR (ATOM (SECOND Y))
                             (NOT (EQ (CAR (SECOND Y)) 'QUOTE))
                             (ATOM (THIRD Y))
                             (NOT (EQ (CAR (THIRD Y)) 'QUOTE))))
                    (LIST 'BQ
                          (LIST 'DOTTED
                                (USEBQ (MAPCAR 
                                        (FUNCTION 
                                         (LAMBDA (Y) 
                                          (COND ((MEMQ Y '(T NIL)) Y) 
                                           ((SYMBOLP Y) (LIST 'COMMA Y)) 
                                           ((ATOM Y) Y) 
                                           ((AND (EQ (CAR Y) 'QUOTE) 
                                             (LISTP (CDR Y)) (NULL (CDDR Y)))
                                            (SECOND Y))
                                           (T (LIST 'COMMA Y)))))
                                        (CDR Y))))))
                   ((AND (EQ (FIRST Y) 'LIST)
                         (SOME (FUNCTION
                             (LAMBDA (Y) (OR (ATOM Y) (NOT (EQ (CAR Y) 'QUOTE)))))
                            (CDR Y)))
                    (LIST 'BQ
                          (USEBQ (MAPCAR (FUNCTION 
                                          (LAMBDA (Y) 
                                           (COND ((MEMQ Y '(T NIL)) Y) 
                                            ((SYMBOLP Y) (LIST 'COMMA Y)) 
                                            ((ATOM Y) Y) 
                                            ((AND (EQ (CAR Y) 'QUOTE) 
                                              (LISTP (CDR Y)) (ATOM (CDDR Y)))
                                             (SECOND Y))
                                            (T (LIST 'COMMA Y)))))
                                    (CDR Y)))))
                   (T Y))))))

(DEFUN ELIMINATE-COMMA (X)
   (COND ((ATOM X) X)
         ((AND (EQ (CAR X) 'COMMA)
               (LISTP (SECOND X))
               (EQ (CAR (SECOND X)) 'BQ))
          (ELIMINATE-COMMA (SECOND (SECOND X))))
         (T (CONS (ELIMINATE-COMMA (CAR X)) (ELIMINATE-COMMA (CDR X))))))

#+ANSI-CL
(DEFUN PUTPROP (SYM VAL KEY)
  (SETF (GET SYM KEY) VAL))

(MAPC (FUNCTION (LAMBDA (X) (PUTPROP X (FUNCTION @FUNCTION) 'PRIND)))
   '(FUNCTION))

(MAPC (FUNCTION (LAMBDA (X) (PUTPROP X (FUNCTION @LAMBDA) 'PRIND)))
   '(LAMBDA LET LETS))

(MAPC (FUNCTION (LAMBDA (X) (PUTPROP X (FUNCTION @DEFUN) 'PRIND)))
   '(DEFUN MACRO DEFMACRO))

(MAPC (FUNCTION (LAMBDA (X) (PUTPROP X (FUNCTION @PROG) 'PRIND))) '(PROG))

(MAPC (FUNCTION (LAMBDA (X) (PUTPROP X (FUNCTION @SETQ) 'PRIND))) '(SETQ))

(MAPC (FUNCTION (LAMBDA (X) (PUTPROP X (FUNCTION @FUNCALL) 'PRIND)))
   '(SELECTQ MATCH ASSQ ASSOC DELQ REMQ MAP MAPLIST MAPCON MAPC MAPCAR MAPCAN 
     MAPV MAPVECTOR SOME EVERY RPLACA RPLACD FUNCALL DEFPROP PUTPROP CATCH 
     THROW))

#+s3600
(REMOB 'COMMA)
#+s3600
(REMOB 'BQ)
#+s3600
(REMOB 'DOTTED)

