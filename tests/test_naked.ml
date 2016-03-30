open Sudoku
open Puzzlemap

open NUnit.Framework

[<Test>]
let ``Can find naked singles``() =
    let sudoku = "000105000140000670080002400063070010900000003010090520007200080026000035000409000" in

    let p = tPuzzleMap PuzzleShape.default' in

    let solution = Load.load PuzzleShape.default' sudoku in

    let candidateReductions = LoadEliminate.find p solution.current in
    let newSolution = LoadEliminate.step p solution candidateReductions in

    let cellCandidates = Solution.currentCellCandidates p.cells newSolution.current in

    let hints = Naked.find 1 p cellCandidates in

    let expectedHints : Hint.description list =
        [   { primaryHouses = Houses.empty;
              secondaryHouses = Houses.empty;
              candidateReductions = [];
              setCellValueAction = Some (Value.make (Cell.make (Column.make 8) (Row.make 1)) (Digits.nth PuzzleShape.default'.alphabet 8));
              pointers = [];
              focus = Digits.empty };
            { primaryHouses = Houses.empty;
              secondaryHouses = Houses.empty;
              candidateReductions = [];
              setCellValueAction = Some (Value.make (Cell.make (Column.make 8) (Row.make 9)) (Digits.nth PuzzleShape.default'.alphabet 5));
              pointers = [];
              focus = Digits.empty } ]
        in

    let _ = Assert.AreEqual(2, List.length hints) in
    Assert.AreEqual(expectedHints, hints)
