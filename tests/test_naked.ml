module test_naked

open Sudoku
open Puzzlemap
open LoadEliminate

open Naked

open Load

open NUnit.Framework

[<Test>]
let ``Can find naked singles``() =
    let sudoku = "000105000140000670080002400063070010900000003010090520007200080026000035000409000" in

    let p = tPuzzleMap PuzzleShape.default' in

    let solution = load PuzzleShape.default' sudoku in

    let candidateReductions = loadEliminateFind p solution.current in
    let newSolution = loadEliminateStep p solution candidateReductions in

    let cellCandidates = Solution.currentCellCandidates p.cells newSolution.current in

    let hints = nakedSingle p cellCandidates in

    let expectedHints : Hints.hintDescription list =
        [   { Hints.hintDescription.primaryHouses = Houses.empty;
              secondaryHouses = Houses.empty;
              candidateReductions = [];
              setCellValueAction = Some (Value.make (Cell.make (Column.make 8) (Row.make 1)) (Digits.nth PuzzleShape.default'.alphabet 8));
              pointers = [];
              focus = Digits.empty };
            { Hints.hintDescription.primaryHouses = Houses.empty;
              secondaryHouses = Houses.empty;
              candidateReductions = [];
              setCellValueAction = Some (Value.make (Cell.make (Column.make 8) (Row.make 9)) (Digits.nth PuzzleShape.default'.alphabet 5));
              pointers = [];
              focus = Digits.empty } ]
        in

    let _ = Assert.AreEqual(2, List.length hints) in
    Assert.AreEqual(expectedHints, hints)
