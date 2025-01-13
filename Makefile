PROJ_TEST := eocia-haskell-test

test-watch:
	ghcid -c "cabal repl $(PROJ_TEST)" -T "Main.main"

dump:
	@git ls-files | while read file; do \
		echo "---- $$file ----" && \
		cat "$$file"; \
	done > dump.txt

.PHONY: test-watch dump
