.PHONY: default
default: build

.PHONY: init
init:
	cp ${ORX}/lib/dynamic/liborx* src/

.PHONY: build
build:
	dune build src/shatter.exe

.PHONY: exec
exec: build
	./src/shatter.exe

.PHONY: check
check:
	dune build @check

.PHONY: watch
watch:
	dune build @check -w

.PHONY: clean
clean:
	dune clean
