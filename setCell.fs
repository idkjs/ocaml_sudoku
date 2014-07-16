module setCell

open System
open System.Text

open sudoku
open puzzlemap
open format
open hints
open console

let setCellValueModelEffect (setCellValue:SetCellValue) (puzzleMaps:PuzzleMaps) (candidateLookup:Cell->Set<Candidate>) : Set<Cell> =
    let houseCells = allHouseCells puzzleMaps setCellValue.cell
    let candidateReductions = Set.filter (fun c -> 
        let cs = candidateLookup c
        Set.contains setCellValue.value cs) houseCells

    candidateReductions

let setACell (setCellValue:SetCellValue) (puzzleMaps:PuzzleMaps) (candidateLookup:Cell->Set<Candidate>) : (Cell->AnnotatedSymbol<AnnotatedCandidate>)->(Cell->AnnotatedSymbol<AnnotatedCandidate>) =
    let candidateReductions = setCellValueModelEffect setCellValue puzzleMaps candidateLookup

    fun (entryLookup:Cell->AnnotatedSymbol<AnnotatedCandidate>) ->
        fun (cell:Cell) ->
            let entry = entryLookup cell
            match entry with
            | Given _ 
            | Set _ -> entry
            | Candidates candidates ->
                if setCellValue.cell = cell then
                    Set (candidateToSymbol setCellValue.value)
                else
                    let f s =
                        let hs = candidates s
                        if candidateReductions.Contains cell && s = setCellValue.value then
                            Removed
                        else
                            hs
                    Candidates f


let setValue (puzzleMaps:PuzzleMaps) (candidate:Candidate) (entryLookup:Cell->AnnotatedSymbol<AnnotatedCandidate>) cell =
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

let setValueToString (setCellValue:SetCellValue) =
    String.Format("SetValue: {0} = {1}", formatCell setCellValue.cell, formatCandidate setCellValue.value)

let setCellCandidateGridPre (setCellValue:SetCellValue) (puzzleMaps:PuzzleMaps) (candidateLookup:Cell->Set<Candidate>):(Cell->AnnotatedSymbol<AnnotatedCandidate>)->(Cell->HintAnnotatedSymbol) =
    let candidateReductionCells = setCellValueModelEffect setCellValue puzzleMaps candidateLookup
    //let candidateReductions = Set.map (fun cell -> {CandidateReduction.cell = cell; symbols = set [setCellValue.value]}) candidateReductionCells
    //let crs = Set.toList candidateReductions

    //setHintId >> setReductions2 crs >> setCellHint setCellValue.cell setCellValue.value candidateLookup
    let ccs = candidateLookup setCellValue.cell
    let ccs2 = Set.remove setCellValue.value ccs
    let crs = {CandidateReduction.cell = setCellValue.cell; symbols = ccs2}

    setHintId >> setReductions setCellValue.value candidateReductionCells >> setReductions2 [crs] >> setCellHint setCellValue.cell setCellValue.value candidateLookup
