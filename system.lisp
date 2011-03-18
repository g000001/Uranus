;;;---*- Mode:LISP; Base: 10.; Syntax: Common-lisp; Package: CL-USER -*---
;;; Uranus initiation file
;;; (c) H. Nakashima and S. Tomura

(in-package :URANUS-USER)

(push ':uranus *features*)

;;; Uranus package family: 
(defpackage uranus (:use cl cl-user) (:nicknames ura))

;;; Amuse package family: 
(defpackage Amuse (:use cl cl-user))
(defpackage ec (:use amuse cl cl-user) (:nicknames e))


(defsystem uranus
  (:pretty-name "Uranus"
   :short-name "Uranus"
   :default-pathname "uranus:system;"
   :patchable t
   :maintaining-sites :etl)
  (:module font "uranus-font" (:type :font))
  (:module system ("systempred" "lispfunctions"))
  (:module debug ("trace" "stepper"))
;  (:module manual "Uranus: system; uranus.manual" (:type :text))
  (:serial
    "decl"
   (:parallel "defs" "lib")
   (:serial "td" "readin" "kernel"
	    (:parallel
	      (:serial "amuse" system)
	      (:serial font "window")
	      debug
	      "zwei-uranus-mode")))

  (:module demo 
   (   
   "uranus:demo;amord.pkr"
   "uranus:demo;arithmetic.ura"
   "uranus:demo;bird.ura"
   "uranus:demo;bird2.ura"
   "uranus:demo;birthday.ura"
   "uranus:demo;example.mrs"
   "uranus:demo;example.uni"
   "uranus:demo;examples.pkr"
   "uranus:demo;frame.pkr"
   "uranus:demo;functional-geometry.ura"
   "uranus:demo;mrs.pkr"
   "uranus:demo;nautilus.ura"
   "uranus:demo;number.stk"
   "uranus:demo;production.ura"
   "uranus:demo;quicksort.pkr"
   "uranus:demo;set.pkr"
   "uranus:demo;shoot.ura"
   "uranus:demo;sieve.ura"
   "uranus:demo;sieve1.ura"
   "uranus:demo;smalltalk.ura"
   "uranus:demo;temporal-logic.ura"
   "uranus:demo;test-cut.ura"
   "uranus:demo;uni.ura"
   ) (:type :text))

)


;  (:load-bfd font)
;  (:compile-load decl   (:load-bfd font)         (:load-bfd font))
;  (:compile-load lib)
;  (:compile-load prind  (:fasload lib))
;  (:compile-load amuse  (:fasload lib #-symbolics prind))
;  (:compile-load td)
;  (:compile-load kernel (:fasload defs lib td))
;  (:compile-load system (:fasload defs kernel ))
;  (:compile-load debug  (:fasload defs kernel))
;  (:compile-load readin (:fasload td))
;  (:compile-load zwei-uranus-mode (:fasload defs))
;  (:compile-load window (:fasload defs kernel system))




