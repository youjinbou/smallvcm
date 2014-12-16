FLAGS= -I src -pkg gmaths -cflag -bin-annot

all: release

release:
	 ocamlbuild $(FLAGS) src/smallvcm.native

debug:
	ocamlbuild $(FLAGS) src/smallvcm.d.byte

test:
	 ocamlbuild $(FLAGS) -pkg oUnit tests/test.native

clean:
	ocamlbuild -clean
