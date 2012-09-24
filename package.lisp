(defpackage :uranus
  (:use :cl)
  (:shadow :assert)
  (:shadow :system)
  (:nicknames :ura)
  (:export :uranus@version)
  (:export :uranus-readtable)
  (:export :@reload-deflist :@reload-flag)
  (:export :@printlevel)
  (:export :uranus)
  (:export :toplevel-execute))


(defpackage :uranus-user
  (:use :cl :cl-user)
  (:nicknames :ura-user)
  (:export :on
	   :off
	   :? :! :\] :??? :% :||
	   :ok
	   :compile 
	   :amuse
	   :assert :assertq :asserta :assertz
	   :as :aq :az :aa :define
	   :deny :denya :denyz :denyq
	   :edit
	   :standard-world
	   :system
	   :standard-input
	   :standard-output
	   :lambda
	   :clause
	   :macro
	   :attention
	   :uranus-system-code )
  ;;; to amuse
  (:export :$$$ :? :n :b :p :pp :i :in :ib :it :k :d :e :c :f :v :fn :r
           :ra :r1 :r2 :r3 :var :z :s :sc :q :o :pop :st
           :stack :top :level :x :l :last :li :ri :bi :bo :u :?
           :member :append :nil
           :* :*element :*rest :*top :*rest :*any :*a :*x :*y :*z) )


(defpackage :uranus.time 
  (:use :cl)
  (:export :microsecond-time
           :get-universal-time
           :fixnum-microsecond-time))


;;; Amuse package family: 
(defpackage Amuse
  (:use :cl :cl-user)
  (:export :getfile
           :putfile 
           :fetchvalue)
  ;; (:shadow :list-length) ???
  (:export :edit-name
           :editor-get-definition
           :editor-restore-definition
           :editor-execute )
  )


(defpackage ec (:use :amuse :cl :cl-user) (:nicknames :e))


(IN-PACKAGE :URANUS)

