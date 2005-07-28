GHC = ghc -O2

.PHONY : helisp
helisp:
	$(GHC) -o helisp  Helisp.hs --make

clean:
	rm -rf *.o
	rm -rf *.hi

