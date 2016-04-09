RESULT = sudoku_repl

SOURCES = \
  src/sset.ml src/smap.ml \
  src/sudoku.mli src/sudoku.ml \
  src/puzzlemap.mli src/puzzlemap.ml \
  src/hint.mli src/hint.ml \
  src/loadEliminate.mli src/loadEliminate.ml \
  src/setCell.mli src/setCell.ml \
  src/eliminateCandidate.mli src/eliminateCandidate.ml \
  src/force.mli src/force.ml \
  src/fullHouse.mli src/fullHouse.ml \
  src/hidden.mli src/hidden.ml \
  src/naked.mli src/naked.ml \
  src/intersection.mli src/intersection.ml \
  src/wing.mli src/wing.ml \
  src/format.mli src/format.ml \
  src/console.mli src/console.ml \
  src/command.mli src/command.ml \
  src/load.mli src/load.ml \
  src/console_nix.mli src/console_nix.ml \
  main.ml

OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
