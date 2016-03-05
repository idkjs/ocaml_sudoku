module core.hints

open System
open System.Text

open sset
open sudoku
open puzzlemap

exception CellStateInvalid

let first (set : digits) = (Digits.toArray set).[0]

let rec doSetSubsets (list : List<'a>) (size : int) (prefix : List<'a>) : List<List<'a>> = 
    match list with
    | x :: xs when size > 0 -> 
        if size = 1 then (x :: prefix) :: doSetSubsets xs 1 prefix
        else 
            let inc = doSetSubsets xs (size - 1) (x :: prefix)
            let dec = doSetSubsets xs size prefix

            List.append inc dec
    | _ -> []

let rec setSubsets (as' : 'a array) (size : int) : SSet<'a> array =
    let list = Array.toList as'
    doSetSubsets list size []
    |> List.map List.toArray
    |> List.map SSet.ofArray
    |> List.toArray

(*
    let s0 = []
    let p00 = setSubsets s0 0
    let p01 = setSubsets s0 1
    let p02 = setSubsets s0 2

    let s1 = [ 1 ]
    let p10 = setSubsets s1 0
    let p11 = setSubsets s1 1
    let p12 = setSubsets s1 2
    let p13 = setSubsets s1 3

    let s2 = [ 1; 2 ]
    let p20 = setSubsets s2 0
    let p21 = setSubsets s2 1
    let p22 = setSubsets s2 2
    let p23 = setSubsets s2 3
    let p24 = setSubsets s2 4

    let s3 = [ 1; 2; 3 ]
    let p30 = setSubsets s3 0
    let p31 = setSubsets s3 1
    let p32 = setSubsets s3 2
    let p33 = setSubsets s3 3
    let p34 = setSubsets s3 4
    let p35 = setSubsets s3 5
*)


type hintDescription = 
    { primaryHouses : houses
      secondaryHouses : houses
      candidateReductions : candidateReductions
      setCellValueAction : value option
      pointers : candidateReductions
      focus : digits }
    override this.ToString() = 
        let sb = StringBuilder()

        sb.AppendLine(String.Format("Primary Houses {0}", String.Join(",", this.primaryHouses))) |> ignore
        sb.AppendLine(String.Format("Secondary Houses {0}", String.Join(",", this.secondaryHouses))) |> ignore
        sb.AppendLine(String.Format("Pointers {0}", String.Join(",", this.pointers))) |> ignore

        this.candidateReductions
        |> CandidateReductions.toArray
        |> Array.iter
            (fun candidateReduction ->
                sb.AppendLine(String.Format("  {0}", candidateReduction)) |> ignore) 

        sb.ToString()

(* To draw a cell we may want to display extra information... *)
type annotation = 
    { given : digit option
      setValue : digit option
      primaryHintHouse : bool
      secondaryHintHouse : bool
      setValueReduction : digit option
      reductions : digits
      pointers : digits
      focus : digits }

[<NoComparisonAttribute>]
type hintDescription2 = 
    { primaryHouses : houses
      secondaryHouses : houses
      candidateReductions : candidateReductions
      setCellValueAction : value option
      annotations : lookup<cell, annotation> }
    override this.ToString() = 
        let sb = StringBuilder()

        sb.AppendLine(String.Format("Primary Houses {0}", String.Join(",", this.primaryHouses))) |> ignore
        sb.AppendLine(String.Format("Secondary Houses {0}", String.Join(",", this.secondaryHouses))) 
        |> ignore

        this.candidateReductions
        |> CandidateReductions.toArray
        |> Array.iter
            (fun candidateReduction ->
                sb.AppendLine(String.Format("  {0}", candidateReduction)) |> ignore) 

        sb.AppendLine(String.Format("Set Cell {0}", this.setCellValueAction)) |> ignore

        sb.ToString()

let mhas (solution : solution) (p : puzzleMap) (hd : hintDescription) : hintDescription2 = 

    let annotationLookup (cell : cell) : annotation = 

        let setValue, setValueReduction = 
            match hd.setCellValueAction with
            | Some setCellValueAction -> 
                
                let r1 = 
                    if setCellValueAction.cell = cell then Some setCellValueAction.digit
                    else None
                
                let r3 = 
                    let cells =
                        p.cellHouseCells.Get setCellValueAction.cell

                    if Cells.contains cell cells then Some setCellValueAction.digit
                    else None
                
                r1, r3
            | None -> None, None
        
        let cellCandidateReductions =
            hd.candidateReductions
            |> CandidateReductions.filter (fun pointer -> cell = pointer.cell) 

        let reductions = 
            match CandidateReductions.firstOpt cellCandidateReductions with
            | Some cr -> cr.candidates
            | _ -> Digits.empty

        let cellPointers =
            hd.pointers
            |> CandidateReductions.filter (fun pointer -> cell = pointer.cell)

        let pointers = 
            match CandidateReductions.firstOpt cellPointers with
            | Some cr -> cr.candidates
            | _ -> Digits.empty
        
        let primaryHouseCells =
            p.housesCells hd.primaryHouses

        let secondaryHouseCells =
            p.housesCells hd.secondaryHouses

        { annotation.given = None
          setValue = setValue
          primaryHintHouse = Cells.contains cell primaryHouseCells
          secondaryHintHouse = Cells.contains cell secondaryHouseCells
          setValueReduction = setValueReduction
          reductions = reductions
          pointers = pointers
          focus = hd.focus }

    let annotations = makeMapLookup p.cells annotationLookup

    { hintDescription2.primaryHouses = hd.primaryHouses
      secondaryHouses = hd.secondaryHouses
      candidateReductions = hd.candidateReductions
      setCellValueAction = hd.setCellValueAction
      annotations = annotations }
