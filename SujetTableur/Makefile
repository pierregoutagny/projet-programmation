SRC_FILES=$(wildcard *.ml *.mly *.mll)
TESTS=$(addsuffix .out, $(basename $(wildcard tests/*.in)))
EXEC=main.native

all: $(EXEC)

$(EXEC): ${SRC_FILES}
	ocamlbuild -yaccflag -v -lib unix $(EXEC) #on dit de fabriquer main.native

byte: 
	ocamlbuild -yaccflag -v main.byte


test: ${TESTS}
	@echo TOUS LES TESTS SONT PASSÉS

tests/%.out: tests/%.in $(EXEC) tests/%.res 
	@echo "Test $*..."
	@./$(EXEC) < $< | diff $(basename $@).res -
	
# Si test.res.paf existe, on teste avec -paf et on compare
	@if [ -f $(basename $@).res.paf ]; \
	then \
		echo "Test $* avec -paf..."; \
		./$(EXEC) -paf < $< | diff $(basename $@).res.paf -; \
	else true; \
	fi

clean: 
	ocamlbuild -clean
