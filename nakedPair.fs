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
                        let hint = { NakedPair.cell1 = cell; cell2 = cell2; candidates = candidates; candidateReductions = reductions; house = house }
                        hints := hint :: !hints
                ) (Seq.skip (i + 1) hht)) hht
    !hints

let findNakedPairs (candidateLookup:Cell->Set<Candidate>) (puzzleMaps:PuzzleMaps) =

    let perHouse = nakedPairPerHouse candidateLookup puzzleMaps

    let houses = allHouses puzzleMaps

    List.collect perHouse houses

let nakedPairToString (hint:NakedPair) =
    let sb = StringBuilder()

    sb.AppendLine (String.Format ("{0}, Cell {1}, Cell {2}, {3}", formatHouse hint.house, formatCell hint.cell1, formatCell hint.cell2, formatCandidates hint.candidates)) |> ignore

    List.iter
        (fun (cr:CandidateReduction) ->
            sb.AppendLine (String.Format ("  {0}", formatCandidateReduction cr)) |> ignore
        )
        hint.candidateReductions

    sb.ToString()

let nakedPairSymbolTo (hint:NakedPair) (puzzleMaps:PuzzleMaps) : (Cell->AnnotatedSymbol<AnnotatedCandidate>)->(Cell->HintAnnotatedSymbol) =
    let houseCells = getHouseCells puzzleMaps hint.house |> Set.ofList
    let pointers = set [hint.cell1; hint.cell2]

    houseSymbolTo houseCells >> setReductions2 hint.candidateReductions >> setPointer pointers hint.candidates
