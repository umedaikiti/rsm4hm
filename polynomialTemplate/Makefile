export OCAMLMAKEFILE = ./OCamlMakefile

LIB = lib

# environment variables for aff_main
AFFSRCDIR = src_aff
AFFSOURCES = $(LIB)/util.mli $(LIB)/util.ml \
$(LIB)/variable.mli $(LIB)/variable.ml \
$(LIB)/coefficient.mli $(LIB)/coefficient.ml \
$(LIB)/polynomial.mli $(LIB)/polynomial.ml \
$(LIB)/semialg.mli $(LIB)/semialg.ml \
$(AFFSRCDIR)/matrix.mli $(AFFSRCDIR)/matrix.ml \
$(LIB)/pcfg.mli $(LIB)/pcfg.ml \
$(LIB)/syntax.mli $(LIB)/syntax.ml \
$(LIB)/lexer.mll $(LIB)/parser.mly \
$(LIB)/sequent.mli $(LIB)/sequent.ml \
$(AFFSRCDIR)/linear_template.mli $(AFFSRCDIR)/linear_template.ml \
$(LIB)/eval.mli $(LIB)/eval.ml \
$(AFFSRCDIR)/progmath.mli $(AFFSRCDIR)/progmath.ml \
$(AFFSRCDIR)/main.ml

AFFRESULT = aff_main

AFFVARS = SOURCES="$(AFFSOURCES)" RESULT=$(AFFRESULT) 

# environment variables for poly_main
POLYSRCDIR = src_poly
POLYSOURCES = $(LIB)/util.mli $(LIB)/util.ml \
$(LIB)/variable.mli $(LIB)/variable.ml \
$(LIB)/coefficient.mli $(LIB)/coefficient.ml \
$(LIB)/polynomial.mli $(LIB)/polynomial.ml \
$(LIB)/semialg.mli $(LIB)/semialg.ml \
$(LIB)/pcfg.mli $(LIB)/pcfg.ml \
$(LIB)/syntax.mli $(LIB)/syntax.ml \
$(LIB)/lexer.mll $(LIB)/parser.mly \
$(LIB)/sequent.mli $(LIB)/sequent.ml \
$(POLYSRCDIR)/polynomial_template.mli $(POLYSRCDIR)/polynomial_template.ml \
$(LIB)/eval.mli $(LIB)/eval.ml \
$(POLYSRCDIR)/sostools.mli $(POLYSRCDIR)/sostools.ml \
$(POLYSRCDIR)/julia_sos.mli $(POLYSRCDIR)/julia_sos.ml \
$(POLYSRCDIR)/poly_main.ml 

POLYRESULT = poly_main

POLYVARS = SOURCES="$(POLYSOURCES)" RESULT=$(POLYRESULT) 


# common environment variables 
VARS = OCAMLLEX=ocamllex OCAMLYACC=menhir YFLAGS=-v 

all:	poly_main

aff_main: $(AFFSOURCES)
	$(MAKE) -f $(OCAMLMAKEFILE) $(AFFVARS) $(VARS)

poly_main: $(POLYSOURCES) 
	$(MAKE) -f $(OCAMLMAKEFILE) $(POLYVARS) $(VARS)

clean:
	$(MAKE) clean -f $(OCAMLMAKEFILE) $(AFFVARS) TRASH="$(AFFSRCDIR)/parser.automaton $(AFFSRCDIR)/parser.conflicts"
	$(MAKE) clean -f $(OCAMLMAKEFILE) $(POLYVARS) TRASH="$(POLYSRCDIR)/poly_parser.automaton $(POLYSRCDIR)/poly_parser.conflicts"
