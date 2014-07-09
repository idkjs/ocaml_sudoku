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

let setACell (setCellValue:SetCellValue) (puzzleMaps:PuzzleMaps) (candidateLookup:Cell->Set<Candidate>) : (Cell->AnnotatedSymbol)->(Cell->AnnotatedSymbol) =
    let candidateReductions = setCellValueModelEffect setCellValue puzzleMaps candidateLookup

    fun (entryLookup:Cell->AnnotatedSymbol) ->
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

let setValueToString (setCellValue:SetCellValue) =
    String.Format("SetValue: {0} = {1}", formatCell setCellValue.cell, formatCandidate setCellValue.value)

let setCellCandidateGridPre (setCellValue:SetCellValue) (puzzleMaps:PuzzleMaps) (candidateLookup:Cell->Set<Candidate>):(Cell->AnnotatedSymbol)->(Cell->HintAnnotatedSymbol) =
    let candidateReductions = setCellValueModelEffect setCellValue puzzleMaps candidateLookup

    fun (toLabel:Cell->AnnotatedSymbol) ->
        fun (cell:Cell) ->
            let l = toLabel cell
            match l with
            | Given _
            | Set _ ->
                HASId l
            | Candidates candidates ->
                let newHC c = 
                    let hc = candidates c

                    if cell = setCellValue.cell then
                        match hc with
                        | Possible when c = setCellValue.value -> HACSet
                        | Possible -> Reduction
                        | _ -> HACId hc
                    else if Set.contains cell candidateReductions && c = setCellValue.value then
                        Reduction
                    else
                        HACId hc

                FLHintCandidates newHC

