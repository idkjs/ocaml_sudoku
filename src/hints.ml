open Sset
open Smap
open Sudoku
open Puzzlemap

exception CellStateInvalid

let first (set : digits) = (Digits.toList set).[0]

let rec doSetSubsets (list : List<'a>) (size : int) (prefix : List<'a>) : List<List<'a>> = 
    match list with
    | x :: xs when size > 0 -> 
        if size = 1 then (x :: prefix) :: doSetSubsets xs 1 prefix
        else 
            let inc = doSetSubsets xs (size - 1) (x :: prefix) in
            let dec = doSetSubsets xs size prefix in

            List.append inc dec
    | _ -> []

let rec setSubsets (as' : 'a list) (size : int) : SSet<'a> list =
    doSetSubsets as' size []
    |> List.map SSet.ofList

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
    { primaryHouses : houses;
      secondaryHouses : houses;
      candidateReductions : candidateReductions;
      setCellValueAction : value option;
      pointers : candidateReductions;
      focus : digits }

let hintDescription_tostring (h : hintDescription) : string =

    let line1 = Printf.sprintf "Primary Houses %s\r\n" (Houses.tostring h.primaryHouses) in
    let line2 = Printf.sprintf "Secondary Houses %s\r\n" (Houses.tostring h.secondaryHouses) in
    let line3 = Printf.sprintf "Pointers %s\r\n" (CandidateReductions.tostring h.pointers) in

    let crlines =
        h.candidateReductions
        |> CandidateReductions.toList
        |> List.map
            (fun candidateReduction ->
                Printf.sprintf "  %s\r\n" (candidateReduction_tostring candidateReduction))
        in

    [ line1; line2; line3; String.concat "," crlines]
    |> String.concat ","

(* To draw a cell we may want to display extra information... *)
type annotation = 
    { given : digit option;
      current: cellContents;
      setValue : digit option;
      primaryHintHouse : bool;
      secondaryHintHouse : bool;
      setValueReduction : digit option;
      reductions : digits;
      pointers : digits;
      focus : digits }

[<NoComparisonAttribute>]
type hintDescription2 = 
    { annotations : SMap<cell, annotation> }

let mhas (solution : solution) (p : puzzleMap) (hd : hintDescription) : hintDescription2 = 

    let annotationLookup (cell : cell) : annotation = 

        let setValue, setValueReduction = 
            match hd.setCellValueAction with
            | Some setCellValueAction -> 
                
                let r1 = 
                    if setCellValueAction.cell = cell then Some setCellValueAction.digit
                    else None
                    in

                let r3 = 
                    let cells = SMap.get p.cellHouseCells setCellValueAction.cell in

                    if Cells.contains cell cells then Some setCellValueAction.digit
                    else None
                    in

                r1, r3
            | None -> None, None
            in

        let cellCandidateReductions =
            hd.candidateReductions
            |> CandidateReductions.filter (fun pointer -> cell = pointer.cell) 
            in

        let reductions = 
            match CandidateReductions.firstOpt cellCandidateReductions with
            | Some cr -> cr.candidates
            | _ -> Digits.empty
            in

        let cellPointers =
            hd.pointers
            |> CandidateReductions.filter (fun pointer -> cell = pointer.cell)
            in

        let pointers = 
            match CandidateReductions.firstOpt cellPointers with
            | Some cr -> cr.candidates
            | _ -> Digits.empty
            in

        let primaryHouseCells =
            p.housesCells hd.primaryHouses
            in

        let secondaryHouseCells =
            p.housesCells hd.secondaryHouses
            in

        { annotation.given = SMap.get solution.given cell;
          current = SMap.get solution.current cell;
          setValue = setValue;
          primaryHintHouse = Cells.contains cell primaryHouseCells;
          secondaryHintHouse = Cells.contains cell secondaryHouseCells;
          setValueReduction = setValueReduction;
          reductions = reductions;
          pointers = pointers;
          focus = hd.focus }
        in

    let annotations = SMap.ofLookup p.cells annotationLookup in

    { hintDescription2.annotations = annotations }

let mhas2 (solution : solution) (p : puzzleMap) : hintDescription2 = 

    let annotationLookup (cell : cell) : annotation = 
        { annotation.given = SMap.get solution.given cell;
          current = SMap.get solution.current cell;
          setValue = None;
          primaryHintHouse = false;
          secondaryHintHouse = false;
          setValueReduction = None;
          reductions = Digits.empty;
          pointers = Digits.empty;
          focus = Digits.empty }
        in

    let annotations = SMap.ofLookup p.cells annotationLookup in

    { hintDescription2.annotations = annotations }
