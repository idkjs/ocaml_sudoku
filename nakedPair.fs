module nakedPair

open System
open System.Text

open sudoku
open puzzlemap
open format
open hints
open console

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
                        let hint = { NakedPair.cell1 = cell; cell2 = cell2; symbols = candidates; candidateReduction = reductions; house = house }
                        hints := hint :: !hints
                ) (Seq.skip (i + 1) hht)) hht
    !hints

let findNakedPairs (candidateLookup:Cell->Set<Candidate>) (puzzleMaps:PuzzleMaps) =

    let perHouse = nakedPairPerHouse candidateLookup puzzleMaps

    let houses = allHouses puzzleMaps

    List.collect perHouse houses

let nakedPairToString (hint:NakedPair) =
    let sb = StringBuilder()

    sb.AppendLine (String.Format ("{0}, Cell {1}, Cell {2}, {3}", formatHouse hint.house, formatCell hint.cell1, formatCell hint.cell2, formatCandidates hint.symbols)) |> ignore

    List.iter
        (fun (cr:CandidateReduction) ->
            sb.AppendLine (String.Format ("  {0}", formatCandidateReduction cr)) |> ignore
        )
        hint.candidateReduction

    sb.ToString()

let nakedPairSymbolTo (hint:NakedPair) (puzzleMaps:PuzzleMaps) : (Cell->AnnotatedSymbol)->(Cell->HintAnnotatedSymbol) =
    let houseCells = getHouseCells puzzleMaps hint.house |> Set.ofList

    fun (etoc:Cell->AnnotatedSymbol) ->
        fun (cell:Cell) ->
            let label = etoc cell

            if cell = hint.cell1 || cell = hint.cell2 then
                match label with
                | Given _
                | Set _ ->
                        HASHouse label  // not used
                | Candidates candidates ->
                    let newHC candidate =
                        let hc = candidates candidate
                        match hc with
                        | Possible -> Pointer
                        | Excluded -> HACId hc
                        | Removed -> HACId hc

                    FLHintCandidates newHC
            else if Set.contains cell houseCells then
                match label with
                | Given _
                | Set _ ->
                        HASHouse label
                | Candidates candidates ->
                        let o = List.tryFind (fun cr -> cell = cr.cell) hint.candidateReduction
                        match o with
                        | Some cr ->
                            let newHC candidate =
                                let hc = candidates candidate
                                match hc with
                                | Possible ->
                                        if Set.contains candidate cr.symbols then
                                            Reduction
                                        else
                                            HACHouse
                                | Excluded -> HACId hc
                                | Removed -> HACId hc

                            FLHintCandidates newHC
                        | _ -> HASHouse label

            else
                HASId label
