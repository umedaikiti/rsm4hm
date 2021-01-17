SOURCES = src/lib/util.mli src/lib/util.ml \
src/variable.mli src/variable.ml \
src/coefficient.mli src/coefficient.ml \
src/affine_term.mli src/affine_term.ml \
src/polytope.mli src/polytope.ml \
src/matrix.mli src/matrix.ml \
src/pcfg.mli src/pcfg.ml \
src/supermartingale.mli src/supermartingale.ml \
src/syntax.mli src/syntax.ml \
src/lexer.mll src/parser.mly \
src/lp_constraint.mli src/lp_constraint.ml \
src/eval.mli src/eval.ml \
src/progmath.mli src/progmath.ml \
src/gen_dot.ml \
src/compile.ml

#PACKS = oUnit
RESULT  = compile

OCAMLLEX = ocamllex
OCAMLYACC = menhir
YFLAGS = -v 

# all: native-code

# all: byte-code byte-code-library
# mod:
# 	./main > example.mod \
# 	glpsol -m example.mod -o example.out


-include OCamlMakefile
