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

type NakedSingle = {
    //candidateReduction : CandidateReduction
    cell : Cell
    symbol : Symbol
}

let findNakedSingles (candidateLookup:CandidateLookup) (cells:Cell list) =

    let candidateCells =
        List.map (fun cell -> (candidateLookup cell, cell)) cells

    let filteredCandidateCells = List.filter (fun (candidates, _) -> Set.count candidates = 1) candidateCells


    List.map (fun (candidates, cell) -> {NakedSingle.cell=cell; NakedSingle.symbol=first candidates }) filteredCandidateCells

let printNakedSingle {NakedSingle.cell = cell; symbol = Symbol symbol} =
    String.Format ("Cell {0}: Symbol: {1}", formatCell cell, symbol)

let nakedSingleSymbolTo (hint:NakedSingle) (etoc:Entry->FormatLabel) (cell:Cell) =
    if cell = hint.cell then
        konst (LHintCell hint.symbol)
    else
        etoc

let nakedSingleFullSymbolTo (hint:NakedSingle) (etoc:Symbol->Entry->FormatLabelF) (cell:Cell) candidate (entry:Entry) =
    match entry with
    | Given symbol ->
        etoc candidate entry
    | Set symbol ->
        etoc candidate entry
    | Candidates(candidates) ->
        if Set.contains candidate candidates &&
            cell = hint.cell then
                FLHintCandidateReduction hint.symbol
        else
            etoc candidate entry

