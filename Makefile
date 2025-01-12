PROJ_TEST := eocia-haskell-test

test-watch:
	ghciwatch --command "cabal repl $(PROJ_TEST)" --test-ghci Main.main --watch test/

dump:
	@git ls-files | while read file; do \
		echo "---- $$file ----" && \
		cat "$$file"; \
	done > dump.txt

.PHONY: test-watch dump
