FLAGS= -classic-display -I src -tag thread -pkgs unix,threads -pkg gmaths -cflag -bin-annot 

all: release

release:
	 ocamlbuild $(FLAGS) src/smallvcm.native

profile:
	 ocamlbuild $(FLAGS) src/smallvcm.p.native

debug:
	ocamlbuild $(FLAGS) src/smallvcm.d.byte

test:
	 ocamlbuild $(FLAGS) -pkg oUnit tests/test.native

clean:
	ocamlbuild -clean
