all: packages uranus

install:
	cp ./uranus /usr/local/bin

packages: defs.o lib.o kernel.o amuse.o systempred.o lispfunctions.o \
	stepper.o trace.o td.o readin.o


defs.o: decl.lisp defs.lisp
	echo '(load "decl.lisp") ;\
	(compile-file "defs.lisp")' | akcl

lib.o: decl.lisp defs.o lib.lisp
	echo '(load "decl.lisp") (load "defs") ;\
	(compile-file "lib.lisp")' | akcl

kernel.o: decl.lisp defs.o lib.o kernel.lisp
	echo '(load "decl.lisp") (load "defs") (load "lib") ;\
	(compile-file "kernel.lisp")' | akcl

amuse.o: decl.lisp amuse.lisp lib.o
	echo '(load "decl.lisp") (load "lib") ;\
	(compile-file "amuse.lisp")' | akcl

systempred.o: decl.lisp defs.o lib.o kernel.o amuse.o systempred.lisp
	echo '(load "decl.lisp") (load "defs") (load "lib") ;\
	(load "kernel") (load "amuse") ;\
	(compile-file "systempred.lisp")' | akcl

lispfunctions.o: decl.lisp defs.o amuse.o lispfunctions.lisp
	echo '(load "decl.lisp") (load "defs") (load "amuse") ;\
	(compile-file "lispfunctions.lisp")' | akcl

stepper.o: decl.lisp defs.o lib.o kernel.o stepper.lisp
	echo '(load "decl.lisp") (load "defs") (load "lib") ;\
	(load "kernel") ;\
	(compile-file "stepper.lisp")' | akcl

trace.o: decl.lisp defs.o lib.o kernel.o trace.lisp
	echo '(load "decl.lisp") (load "defs") (load "lib") ;\
	(load "kernel") ;\
	(compile-file "trace.lisp")' | akcl

td.o: decl.lisp defs.o lib.o td.lisp
	echo '(load "decl.lisp") (load "defs") (load "lib") ;\
	(compile-file "td.lisp")' | akcl

readin.o: decl.lisp defs.o readin.lisp
	echo '(load "decl.lisp") (load "defs") ;\
	(compile-file "readin.lisp")' | akcl

uranus: packages
	echo '(load "make-uranus.lisp") ;\
	(si:save-system "uranus")' | akcl


