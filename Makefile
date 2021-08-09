MODULES=clue clue2 command main authors rooms player_state game_state2
#  game_state player_state rooms
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
	OCAMLBUILD=ocamlbuild -use-ocamlfind \
	-plugin-tag 'package(bisect_ppx-ocamlbuild)'

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

play:
	$(OCAMLBUILD) -tag 'debug' $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

check:
	@bash check.sh
	
finalcheck:
	@bash check.sh final

bisect: clean test
	bisect-ppx-report html

zip:
	zip clue.zip *.ml* *.json *.md _tags .merlin .ocamlformat .ocamlinit LICENSE Makefile	
	
docs: docs-public docs-private
	
docs-public: build
	mkdir -p _doc.public
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal $(PKGS) \
		-html -stars -d _doc.public $(MLIS)

docs-private: build
	mkdir -p _doc.private
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal $(PKGS)\
		-html -stars -d _doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf _doc.public _doc.private clue.zip
