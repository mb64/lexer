# I can never remember the magic incantations to get Haskell to work on Arch

configure:
	cabal new-configure --disable-library-vanilla --enable-shared --enable-executable-dynamic --ghc-options=-dynamic
