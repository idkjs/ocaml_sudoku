open Sudoku
open Puzzlemap

open NUnit.Framework

[<Test>]
let ``Can find full house``() =
    let sudoku = "800739006370465000040182009000600040054300610060500000400853070000271064100940002" in

    let p = tPuzzleMap PuzzleShape.default' in

    let solution = Load.load PuzzleShape.default' sudoku in

    let candidateReductions = LoadEliminate.find p solution.current in
    let newSolution = LoadEliminate.step p solution candidateReductions in

    let cellCandidates = Solution.currentCellCandidates p.cells newSolution.current in

    let hints = FullHouse.find p cellCandidates in

    let expectedHints : Hint.description list =
        [   { primaryHouses = Houses.singleton (HBox (Box.make (Stack.make 2) (Band.make 3)));
              secondaryHouses = Houses.empty;
              candidateReductions = [];
              setCellValueAction = Some (Value.make (Cell.make (Column.make 6) (Row.make 9)) (Digits.nth PuzzleShape.default'.alphabet 5));
              pointers = [];
              focus = Digits.empty } ]
        in

    let _ = Assert.AreEqual(1, List.length hints) in
    Assert.AreEqual(expectedHints, hints)
