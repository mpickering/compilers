# lab2/Makefile

## Add your own test cases to this list
TEST = gcd simple fac0 fac sumpow sumpow2 digits cpsfib
 
 

KEIKO = ../keiko

all: hppc

hppc: prep alex happy
	cabal build && cp dist/build/hppc/hppc ./
	

prep:
	cabal sandbox init && cabal install --only-dependencies

alex:
	alex -o src/Lexer.hs src/Lexer.x 

happy:
	happy -o src/Parse.hs src/Parse.y

test: all 
	$(MAKE) $(TEST:%=test-%)

test-%: 
	@echo "*** Test $*.p"
	./hppc tests/$*.p >a.k
	$(KEIKO)/pplink -custom -nostdlib $(KEIKO)/lib.k a.k -o a.x >/dev/null
	$(KEIKO)/ppx ./a.x >a.test
	sed -n -e '1,/^(\*<</d' -e '/^>>\*)/q' -e p tests/$*.p | diff - a.test
	@echo "*** Passed"; echo

veryclean: clean
	cabal sandbox delete
	rm -rf dist

clean: force
	rm -f src/Parse.hs src/Lexer.hs
	rm -f hppc
	rm -f a.k a.out a.x a.test






force:	
