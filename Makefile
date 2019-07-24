.PHONY: clean console test

build:
	dune build

clean:
	dune clean

exec:
	dune exec ./src/$(TARGET)/bin/main.exe

console:
	dune utop ./src

test:
	dune runtest ./test

watch:
	dune build -w
