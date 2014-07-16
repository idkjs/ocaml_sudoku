module nakedSingle

// Naked Single means:
// For a cell there is only one candidate

open System
open System.Text

open sudoku
open puzzlemap
open format
open hints
open console

let findNakedSingles (candidateLookup:Cell->Set<Candidate>) (cells:Cell list) =

    let candidateCells =
        List.map (fun cell -> (candidateLookup cell, cell)) cells

    let filteredCandidateCells = List.filter (fun (candidates, _) -> Set.count candidates = 1) candidateCells


    List.map (fun (candidates, cell) -> {NakedSingle.cell=cell; NakedSingle.symbol=first candidates }) filteredCandidateCells

let printNakedSingle {NakedSingle.cell = cell; symbol = Candidate symbol} =
    String.Format ("Cell {0}: Symbol: {1}", formatCell cell, symbol)

let nakedSingleSymbolTo (hint:NakedSingle) (candidateLookup:Cell->Set<Candidate>) : (Cell->AnnotatedSymbol<AnnotatedCandidate>)->(Cell->HintAnnotatedSymbol) =
    setHint (set [hint.cell]) >> setCellHint hint.cell hint.symbol candidateLookup
