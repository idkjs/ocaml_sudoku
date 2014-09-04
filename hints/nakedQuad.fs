module hints.nakedQuad

open System
open System.Text

open console

open core.sudoku
open hints

type NakedQuad = 
    { candidateReductions : Set<CandidateReduction>
      pointers : Set<CandidateReduction>
      house : House }
    override this.ToString() = 
        let sb = StringBuilder()

        let cells = Set.toArray (Set.map (fun p -> p.cell) this.pointers)
        let candidates = Set.unionMany (Set.toArray (Set.map (fun p -> p.symbols) this.pointers))

        sb.AppendLine
            (String.Format
                 ("nq {0}, Cells {1}, {2}", this.house, String.Join(",", cells), 
                  String.Join(",", Set.toArray candidates))) |> ignore
        Set.iter (fun (cr : CandidateReduction) -> sb.AppendLine(String.Format("  {0}", cr)) |> ignore) 
            this.candidateReductions

        sb.ToString()

let nakedQuadPerHouse (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) (house : House) = 
    let cells = houseCells house

    let candidateCells = Set.map (fun cell -> ((candidateLookup cell), cell)) cells

    let hht = Set.filter (fun (candidates, _) -> Set.count candidates > 0 && Set.count candidates <= 4) candidateCells

    let hints = ref []

    Seq.iteri 
        (fun i (candidates1, cell1) -> 
        Seq.iteri (fun j (candidates2, cell2) -> 
            let pair = Set.union candidates1 candidates2
            if Set.count pair <= 4 then 
                Seq.iteri (fun k (candidates3, cell3) -> 
                    let triple = Set.union pair candidates3
                    if Set.count triple <= 4 then 
                        Seq.iteri (fun k (candidates4, cell4) -> 
                            let quad = Set.union triple candidates4
                            if Set.count quad <= 4 then 
                                // find any reductions
                                let reductionCandidates = 
                                    Set.map (fun (candidatesrc, cellrc) -> 
                                        { CandidateReduction.symbols = Set.intersect quad candidatesrc
                                          cell = cellrc }) candidateCells
                                
                                let reductions = 
                                    Set.filter 
                                        (fun { CandidateReduction.symbols = candidatesrc; cell = cellrc } -> 
                                        cell1 <> cellrc && cell2 <> cellrc && cell3 <> cellrc && cell4 <> cellrc 
                                        && Set.count candidatesrc > 0) reductionCandidates
                                
                                let makeCandidateReduction cell = 
                                    { CandidateReduction.cell = cell
                                      symbols = candidateLookup cell }

                                if reductions.Count > 0 then 
                                    let hint = 
                                        { NakedQuad.candidateReductions = reductions
                                          pointers = 
                                              set [ makeCandidateReduction cell1
                                                    makeCandidateReduction cell2
                                                    makeCandidateReduction cell3
                                                    makeCandidateReduction cell4 ]
                                          house = house }
                                    hints := hint :: !hints) (Seq.skip (i + j + k + 3) hht)) (Seq.skip (i + j + 2) hht)) 
            (Seq.skip (i + 1) hht)) hht
    !hints

let nakedQuadFind (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) (houses : House list) = 
    List.collect (nakedQuadPerHouse candidateLookup houseCells) houses

let nakedQuadToDescription (hint : NakedQuad) : HintDescription = 
    { HintDescription.house = Some hint.house
      candidateReductions = hint.candidateReductions
      setCellValue = None
      pointers = hint.pointers }


