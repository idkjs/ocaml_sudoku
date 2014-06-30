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
    symbol : Candidate
}

let findNakedSingles (candidateLookup:Cell->Set<Candidate>) (cells:Cell list) =

    let candidateCells =
        List.map (fun cell -> (candidateLookup cell, cell)) cells

    let filteredCandidateCells = List.filter (fun (candidates, _) -> Set.count candidates = 1) candidateCells


    List.map (fun (candidates, cell) -> {NakedSingle.cell=cell; NakedSingle.symbol=first candidates }) filteredCandidateCells

let printNakedSingle {NakedSingle.cell = cell; symbol = Candidate symbol} =
    String.Format ("Cell {0}: Symbol: {1}", formatCell cell, symbol)

let nakedSingleSymbolTo (hint:NakedSingle) : (Cell->AnnotatedSymbol)->(Cell->FormatLabel) =
    fun (etoc:Cell->AnnotatedSymbol) ->
        fun cell ->
            if cell = hint.cell then
                LHintCell (candidateToSymbol hint.symbol)
            else
                LPlain (etoc cell)

let nakedSingleFullSymbolTo (hint:NakedSingle) : (Cell->Candidate->EntryLabel)->(Cell->Candidate->FormatLabelF) =
    fun (etoc:Cell->Candidate->EntryLabel) ->
        fun (cell:Cell) (candidate:Candidate) ->
            let label = etoc cell candidate
            match label with
            | EGiven _
            | ESet _ ->
                FLPlain label
            | EFLCandidatePossible symbol ->
                if cell = hint.cell then
                    FLHintCell (candidateToSymbol hint.symbol)
                else
                    FLPlain label
            | EFLCandidateExcluded symbol -> FLPlain label

