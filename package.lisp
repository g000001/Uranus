(DEFPACKAGE :URANUS
  (:USE :CL)
  (:SHADOW :ASSERT)
  (:NICKNAMES :URA)
  (:EXPORT :uranus@version))

(DEFPACKAGE :URANUS-USER
  (:USE :CL :CL-USER)
  (:NICKNAMES :URA-USER))

;;; Amuse package family: 
(defpackage Amuse (:use :cl :cl-user))
(defpackage ec (:use :amuse :cl :cl-user) (:nicknames :e))

(IN-PACKAGE :URANUS)
(defvar uranus@version "V-19.8")




