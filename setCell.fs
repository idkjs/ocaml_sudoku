module setCell

open System
open System.Text

open sudoku
open puzzlemap
open format
open console

let setCellValueModelEffect (setCellValue:SetCellValue) (puzzleMaps:PuzzleMaps) (candidateLookup:Cell->Set<Candidate>) : Set<Cell> =
    let houseCells = allHouseCells puzzleMaps setCellValue.cell
    let candidateReductions = Set.filter (fun c -> 
        let cs = candidateLookup c
        Set.contains setCellValue.value cs) houseCells

    candidateReductions

let setACell (setCellValue:SetCellValue) (puzzleMaps:PuzzleMaps) (candidateLookup:Cell->Set<Candidate>) : EntryLookup->EntryLookup =
    let candidateReductions = setCellValueModelEffect setCellValue puzzleMaps candidateLookup

    fun (entryLookup:EntryLookup) ->

        fun (cell:Cell) ->
            if setCellValue.cell = cell then
                Set(candidateToSymbol setCellValue.value)
            else
                let entry = entryLookup cell

                match entry with
                | Candidates candidates when candidateReductions.Contains cell ->
                    Candidates(fun s -> if s = setCellValue.value then Removed else candidates s)
                | _ -> entry


let setValue (puzzleMaps:PuzzleMaps) (candidate:Candidate) (entryLookup:Cell->AnnotatedSymbol) cell =
    match entryLookup cell with
    | Given s ->
        Console.WriteLine ("Cell {0} has been given value {1}", formatCell cell, formatSymbol s)
        None
    | Set s ->
        Console.WriteLine ("Cell {0} has been set value {1}", formatCell cell, formatSymbol s)
        None
    | Candidates _ ->
        Some
            {
                SetCellValue.cell = cell
                value = candidate
            }

let setCellCandidateGrid (setCellValue:SetCellValue) (puzzleMaps:PuzzleMaps) (candidateLookup:Cell->Set<Candidate>):(Cell->Candidate->EntryLabel)->(Cell->Candidate->FormatLabelF) =
    let candidateReductions = setCellValueModelEffect setCellValue puzzleMaps candidateLookup

    fun (toLabel:Cell->Candidate->EntryLabel) ->
        fun (cell:Cell) (candidate:Candidate) ->
            if cell = setCellValue.cell then
                FLPlain (ESet (candidateToSymbol setCellValue.value))
            else if Set.contains cell candidateReductions && candidate = setCellValue.value  then
                FLHintCandidateReduction candidate
            else
                FLPlain (toLabel cell candidate)
