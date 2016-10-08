open Sudoku
open Puzzlemap

exception CellStateInvalid

type description = 
    { primaryHouses : houses;
      secondaryHouses : houses;
      candidateReductions : candidateReduction list;
      setCellValueAction : value option;
      pointers : candidateReduction list;
      focus : digits }

module Description = struct
    let to_string (h : description) : string =

        let line1 = Printf.sprintf "Primary Houses %s\r\n" (Houses.to_string h.primaryHouses) in
        let line2 = Printf.sprintf "Secondary Houses %s\r\n" (Houses.to_string h.secondaryHouses) in
        let line3 = Printf.sprintf "Pointers %s\r\n" (CandidateReductions.to_string h.pointers) in

        let crlines =
            h.candidateReductions
            |> List.map
                (fun candidateReduction ->
                    Printf.sprintf "  %s\r\n" (CandidateReduction.to_string candidateReduction))
            in

        [ line1; line2; line3; String.concat "," crlines]
        |> String.concat ","
end

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

type description2 = 
    { annotations : (cell * annotation) list }

let mhas (solution : solution) (p : puzzleMap) (hd : description) : description2 = 

    let annotationLookup (cell : cell) : annotation = 

        let setValue, setValueReduction = 
            match hd.setCellValueAction with
            | Some setCellValueAction -> 
                
                let r1 = 
                    if setCellValueAction.cell = cell then Some setCellValueAction.digit
                    else None
                    in

                let r3 = 
                    let cells = Smap.get Cell.comparer setCellValueAction.cell p.cellHouseCells in

                    if Cells.contains cell cells then Some setCellValueAction.digit
                    else None
                    in

                r1, r3
            | None -> None, None
            in

        let cellCandidateReductions =
            hd.candidateReductions
            |> List.filter (fun pointer -> cell = pointer.cell) 
            in

        let reductions = 
            match cellCandidateReductions with
            | cr :: _ -> cr.candidates
            | [] -> Digits.empty
            in

        let cellPointers =
            hd.pointers
            |> List.filter (fun pointer -> cell = pointer.cell)
            in

        let pointers = 
            match cellPointers with
            | cr :: _ -> cr.candidates
            | [] -> Digits.empty
            in

        let primaryHouseCells =
            p.housesCells hd.primaryHouses
            in

        let secondaryHouseCells =
            p.housesCells hd.secondaryHouses
            in

        { given = Given.get cell solution.given;
          current = Current.get cell solution.current;
          setValue = setValue;
          primaryHintHouse = Cells.contains cell primaryHouseCells;
          secondaryHintHouse = Cells.contains cell secondaryHouseCells;
          setValueReduction = setValueReduction;
          reductions = reductions;
          pointers = pointers;
          focus = hd.focus }
        in

    let annotations = Cells.ofLookup annotationLookup p.cells in

    { annotations = annotations }

let mhas2 (solution : solution) (p : puzzleMap) : description2 = 

    let annotationLookup (cell : cell) : annotation = 
        { given = Given.get cell solution.given;
          current = Current.get cell solution.current;
          setValue = None;
          primaryHintHouse = false;
          secondaryHintHouse = false;
          setValueReduction = None;
          reductions = Digits.empty;
          pointers = Digits.empty;
          focus = Digits.empty }
        in

    let annotations = Cells.ofLookup annotationLookup p.cells in

    { annotations = annotations }
