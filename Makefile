.PHONY: FORCE clean
all: out/main
out/main: out/main.o out/runtime.o
	gcc -o $@ $^
out/main.o: out/main.s
	as -o $@ $<
out/main.s: FORCE
	cabal run > $@
out/runtime.o: out/runtime.c
	gcc -std=c99 -o $@ -c $<
clean:
	rm -f out/main out/main.o out/main.s out/runtime.o
FORCE:
