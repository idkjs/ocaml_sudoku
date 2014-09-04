module hints.nakedPair

open System
open System.Text

open console

open core.sudoku
open hints

type NakedPair = 
    { candidateReductions : Set<CandidateReduction>
      pointers : Set<CandidateReduction>
      house : House }
    override this.ToString() = 
        let sb = StringBuilder()

        let cells = Set.toArray (Set.map (fun p -> p.cell) this.pointers)
        let candidates = Set.unionMany (Set.toArray (Set.map (fun p -> p.symbols) this.pointers))

        sb.AppendLine
            (String.Format
                 ("np {0}, Cells {1}, {2}", this.house, String.Join(",", cells), 
                  String.Join(",", Set.toArray candidates))) |> ignore
        Set.iter (fun (cr : CandidateReduction) -> sb.AppendLine(String.Format("  {0}", cr)) |> ignore) 
            this.candidateReductions

        sb.ToString()

let nakedPairPerHouse (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) (house : House) = 
    let cells = houseCells house

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

                
                let makeCandidateReduction cell = 
                    { CandidateReduction.cell = cell
                      symbols = candidateLookup cell }

                if reductions.Count > 0 then 
                    let hint = 
                        { NakedPair.candidateReductions = reductions
                          pointers = set [ makeCandidateReduction cell; makeCandidateReduction cell2 ]
                          house = house }
                    hints := hint :: !hints) (Seq.skip (i + 1) hht)) hht
    !hints

let nakedPairFind (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) (houses : House list) = 
    List.collect (nakedPairPerHouse candidateLookup houseCells) houses

let nakedPairToDescription (hint : NakedPair) : HintDescription = 
    { HintDescription.house = Some hint.house
      candidateReductions = hint.candidateReductions
      setCellValue = None
      pointers = hint.pointers }
