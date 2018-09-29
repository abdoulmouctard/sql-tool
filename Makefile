# Nom du projet - sert de nom au fichier executable généré
EXEC=project

# Liste des sources du projet, données dans l'ordre
SOURCES=\
	 value.ml      \
	 relation.mli  \
	relation.ml   \
	moduleR.ml		\
	env.mli       \
	env.ml        \
	ast.mli       \
	ast.ml        \
	parser.mly  	\
	lexer.mll   	\
	main.ml

# Partie générique à ne pas modifier
OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
OCAMLDEP=ocamldep

LEXSRC=$(filter %.mll,$(SOURCES))
YACCSRC=$(filter %.mly,$(SOURCES))
MLSRC=$(SOURCES:.mll=.ml)
MLSRC:=$(MLSRC:.mly=.ml)

OBJECTS=$(MLSRC:.ml=.cmo)
OBJECTS:=$(OBJECTS:.mli=.cmi)

TRASH= $(EXEC) $(YACCSRC:.mly=.ml) $(YACCSRC:.mly=.mli) $(YACCSRC:.mly=.output) $(LEXSRC:.mll=.ml)
TRASH+= $(OBJECTS) $(OBJECTS:.cmo=.cmi)
TRASH+= depend

.PHONY: default clean
.SUFFIXES: .mly .mll .ml .mli .cmo .cmi .cmx

default: $(EXEC)

$(EXEC): $(OBJECTS)
	ocamlc -o $@ $(filter-out %cmi,$^)
#--strict
.mly.ml:
	$(OCAMLYACC) -v $<
	@rm -rf $(patsubst %.mly,%.mli,$<)

.mll.ml:
	$(OCAMLLEX) -o $@ $<

.ml.cmo:
	$(OCAMLC) -o $@ -c $<

.mli.cmi:
	$(OCAMLC) -o $@ -c $<

.ml.cmx:
	$(OCAMLOPT) -o $@ -c $<

clean:
	rm -rf $(TRASH)

# Dependances
depend: $(MLSRC)
	$(OCAMLDEP) $(INCLUDES) $(MLSRC) > depend

-include depend
