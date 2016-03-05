module core.naked

open core.sudoku
open core.puzzlemap
open core.loadEliminate

open hints.naked

open input.load

open NUnit.Framework

[<Test>]
let ``Can find naked singles``() =
    let sudoku = "000105000140000670080002400063070010900000003010090520007200080026000035000409000"

    let p = tPuzzleMap defaultPuzzleSpec :> puzzleMap

    let solution = load defaultPuzzleSpec sudoku

    let candidateReductions = loadEliminateFind p solution.current
    let newSolution = loadEliminateStep p solution candidateReductions

    let cellCandidates = currentCellCandidates p.cells newSolution.current

    let hints = nakedSingle p cellCandidates

    Assert.AreEqual(2, hints.Length)

    let expectedHints : hints.hintDescription array =
        [|
            { hints.hintDescription.primaryHouses = Houses.empty
              secondaryHouses = Houses.empty
              candidateReductions = CandidateReductions.empty
              setCellValueAction = Some { value.cell = { cell.col = makeColumn 8; row = makeRow 1}; digit = defaultPuzzleSpec.alphabet.[8] }
              pointers = CandidateReductions.empty
              focus = Digits.empty };
            { hints.hintDescription.primaryHouses = Houses.empty
              secondaryHouses = Houses.empty
              candidateReductions = CandidateReductions.empty
              setCellValueAction = Some { value.cell = { cell.col = makeColumn 8; row = makeRow 9}; digit = defaultPuzzleSpec.alphabet.[5] }
              pointers = CandidateReductions.empty
              focus = Digits.empty }
        |]

    Assert.AreEqual(expectedHints, hints)
