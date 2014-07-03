module nakedPair

open System
open System.Text

open sudoku
open puzzlemap
open format
open hints
open console

type NakedPair = {
    cell1 : Cell
    cell2 : Cell
    symbols : Set<Candidate>
    candidateReduction : CandidateReduction list
    house : House
    houseCells : Set<Cell>
}

let nakedPairPerHouse (candidateLookup:Cell->Set<Candidate>) (puzzleMaps:PuzzleMaps) (house:House) =

    let cells = getHouseCells puzzleMaps house

    let candidateCells = cells |> List.map (fun cell -> ((candidateLookup cell), cell))

    let hhs = List.filter (fun (candidates, _) -> Set.count candidates = 2) candidateCells

    let hht = Seq.ofList hhs

    let hints = ref []

    Seq.iteri (
        fun i (candidates, cell) ->
        Seq.iter (
            fun (candidates2, cell2) ->
                if candidates = candidates2 then
                    // find any reductions
                    let reductionCandidates =
                        List.map (
                            fun (candidates3, cell3) ->
                                { CandidateReduction.symbols = Set.intersect candidates candidates3; cell = cell3})
                            candidateCells

                    let reductions =
                        List.filter (
                            fun { CandidateReduction.symbols = candidates3; cell = cell3} ->
                                cell <> cell3 && cell2 <> cell3 && Set.count candidates3 > 0)
                            reductionCandidates

                    if reductions.Length > 0 then
                        let hint = { NakedPair.cell1 = cell; cell2 = cell2; symbols = candidates; candidateReduction = reductions; house = house; houseCells = Set.ofList cells }
                        hints := hint :: !hints
                ) (Seq.skip (i + 1) hht)) hht
    !hints

let findNakedPairs (candidateLookup:Cell->Set<Candidate>) (puzzleMaps:PuzzleMaps) =

    let perHouse = nakedPairPerHouse candidateLookup puzzleMaps

    let houses = allHouses puzzleMaps

    List.collect perHouse houses

let printNakedPair {NakedPair.cell1 = cell1; cell2 = cell2; symbols = symbols; candidateReduction = candidateReduction; house = house} =
    Console.WriteLine ("{0}, Cell {1}, Cell {2}, {3}", formatHouse house, formatCell cell1, formatCell cell2, formatCandidates symbols)

    List.iter
        (fun {CandidateReduction.symbols = candidates; cell = cell} ->
            Console.WriteLine ("  Cell {0}, Candidates {1}", formatCell cell, formatCandidates candidates)
        )
        candidateReduction

let nakedPairSymbolTo (hint:NakedPair) : (Cell->Candidate->AnnotatedSymbol)->(Cell->Candidate->FormatLabelF) =
    fun (etoc:Cell->Candidate->AnnotatedSymbol) ->
        fun (cell:Cell) (candidate:Candidate) ->
            let label = etoc cell candidate
            match label with
            | Given _
            | Set _ ->
                if Set.contains cell hint.houseCells then
                    FLHintHouse label
                else
                    FLPlain label
            | Candidates candidates ->
                match candidates candidate with
                | Possible ->
                    if cell = hint.cell1 || cell = hint.cell2 then
                        //FLHintCandidatePointer candidate
                        FLHintCandidates (fun _ -> Pointer)
                    else
                        let o = List.tryFind (fun { CandidateReduction.cell = cell2; symbols = symbols } -> cell = cell2) hint.candidateReduction
                        match o with
                        | Some { CandidateReduction.cell = cell2; symbols = symbols } when Set.contains candidate symbols ->
                            //FLHintCandidateReduction candidate
                            FLHintCandidates (fun _ -> Reduction)
                        | _ ->
                            FLPlain label
                | Excluded -> FLPlain label
                | Removed -> FLPlain label
