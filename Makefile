PROJ_TEST := eocia-haskell-test

test-watch:
	ghciwatch --command "cabal repl $(PROJ_TEST)" --test-ghci Main.main --watch test/

.PHONY: test-watch
