
(load "decl.l")

(compile-file "defs.l")
(load "defs.o")

(in-package 'user)

(compile-file "lib.l")
(load "lib.o")

(compile-file "kernel.l")
(load "kernel.o")

(compile-file "amuse.l")
(load "amuse.o")

(compile-file "systempred.l")
(load "systempred.o")

(compile-file "lispfunctions.l")
(load "lispfunctions.o")

(compile-file "td.l")
(load "td.o")

(compile-file "stepper.l")
(load "stepper.o")

(compile-file "trace.l")
(load "trace.o")

(compile-file "readin.l")
(load "readin.o")




