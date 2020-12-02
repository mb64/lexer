graph:
	cabal build
	./dist-newstyle/build/x86_64-linux/ghc-8.10.2/lexer-0.1.0.0/x/lexer/build/lexer/lexer | dot -Tx11

# I can never remember the magic incantations to get Haskell to work on Arch
configure:
	cabal new-configure --disable-library-vanilla --enable-shared --enable-executable-dynamic --ghc-options="-dynamic -Wall -Wextra"

hlint:
	hlint src
