TESTS=$(addsuffix .out, $(basename $(wildcard tests/*.in)))
EXEC=main.native


all: $(EXEC)

$(EXEC):
	ocamlbuild -yaccflag -v -lib unix $(EXEC) #on dit de fabriquer main.native

byte: 
	ocamlbuild -yaccflag -v main.byte


test: ${TESTS}
	@echo TOUS LES TESTS SONT PASSÉS

tests/%.out: tests/%.in $(EXEC) tests/%.res
	@echo "Test $*..."
	@./$(EXEC) < $< | diff $(basename $@).res -


clean: 
	ocamlbuild -clean
