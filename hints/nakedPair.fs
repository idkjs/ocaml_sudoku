module hints.nakedPair

open System
open System.Text

open console

open core.puzzlemap
open core.sudoku
open hints

type NakedPair = 
    { cell1 : Cell
      cell2 : Cell
      candidates : Set<Candidate>
      candidateReductions : Set<CandidateReduction>
      house : House }
    override this.ToString() = 
        let sb = StringBuilder()

        sb.AppendLine
            (String.Format
                 ("np {0}, Cell {1}, Cell {2}, {3}", this.house, this.cell1, this.cell2, 
                  String.Join(",", Set.toArray this.candidates))) |> ignore
        Set.iter (fun (cr : CandidateReduction) -> sb.AppendLine(String.Format("  {0}", cr)) |> ignore) 
            this.candidateReductions

        sb.ToString()

let nakedPairPerHouse (candidateLookup : Cell -> Set<Candidate>) (puzzleMaps : PuzzleMaps) (house : House) = 
    let cells = getHouseCells puzzleMaps house

    let candidateCells = Set.map (fun cell -> ((candidateLookup cell), cell)) cells

    let hht = Set.filter (fun (candidates, _) -> Set.count candidates = 2) candidateCells

    let hints = ref []

    Seq.iteri (fun i (candidates, cell) -> 
        Seq.iter (fun (candidates2, cell2) -> 
            if candidates = candidates2 then 
                // find any reductions
                let reductionCandidates = 
                    Set.map (fun (candidates3, cell3) -> 
                        { CandidateReduction.symbols = Set.intersect candidates candidates3
                          cell = cell3 }) candidateCells
                
                let reductions = 
                    Set.filter 
                        (fun { CandidateReduction.symbols = candidates3; cell = cell3 } -> 
                        cell <> cell3 && cell2 <> cell3 && Set.count candidates3 > 0) reductionCandidates

                if reductions.Count > 0 then 
                    let hint = 
                        { NakedPair.cell1 = cell
                          cell2 = cell2
                          candidates = candidates
                          candidateReductions = reductions
                          house = house }
                    hints := hint :: !hints) (Seq.skip (i + 1) hht)) hht
    !hints

let nakedPairFind (candidateLookup : Cell -> Set<Candidate>) (puzzleMaps : PuzzleMaps) = 
    let perHouse = nakedPairPerHouse candidateLookup puzzleMaps

    let houses = allHouses puzzleMaps

    List.collect perHouse houses

let nakedPairToDescription (hint : NakedPair) (puzzleMaps : PuzzleMaps) : HintDescription = 
    let pointers = set [ hint.cell1; hint.cell2 ]

    { HintDescription.house = Some hint.house
      candidateReductions = hint.candidateReductions
      setCellValue = None
      pointerCells = pointers
      pointerCandidates = hint.candidates }
