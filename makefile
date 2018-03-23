
lisp.30:
	clisp -C 030/main.lisp

lisp.29:
	clisp -C 029/main.lisp

lisp.27:
	clisp -C 027/main.lisp



all:
	make tmp.exe

tmp.exe: tmp.hs
	ghc --make tmp.hs -outputdir build

clean:
	rm tmp.exe

