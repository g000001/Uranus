;;; -*- Syntax: Common-lisp -*-
(in-package :URANUS-USER)


(push :uranus *features*)


#+SYMBOLICS
(set-dispatch-macro-character #\# #\^
  (get-dispatch-macro-character #\# #\\))

