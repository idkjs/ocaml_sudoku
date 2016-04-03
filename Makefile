RESULT = sudoku_repl
SOURCES = \
  src/sset.ml src/smap.ml \
  src/sudoku.mli src/sudoku.ml \
  src/puzzlemap.mli src/puzzlemap.ml \
  src/hint.mli src/hint.ml \
  src/loadEliminate.mli src/loadEliminate.ml \
  src/setCell.mli src/setCell.ml \
  src/eliminateCandidate.mli src/eliminateCandidate.ml \
  src/force.mli src/force.ml

OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
