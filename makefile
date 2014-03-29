all:
	cabal sandbox init && cabal install && \
	alex Lexer.x && \
	happy Parse.y 
