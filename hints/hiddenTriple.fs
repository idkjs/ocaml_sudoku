module hints.hiddenTriple

// Hidden Pair means:
// For a house and a symbol: there is only one cell in the house with this symbol as a candidate
open System
open System.Text

open console

open core.setCell
open core.sudoku
open hints

type HiddenTriple = 
    { candidateReductions : Set<CandidateReduction>
      pointers : Set<CandidateReduction>
      house : House }
    override this.ToString() = 
        let sb = StringBuilder()

        let cells = Set.toArray (Set.map (fun p -> p.cell) this.pointers)
        let candidates = Set.unionMany (Set.toArray (Set.map (fun p -> p.symbols) this.pointers))

        sb.AppendLine
            (String.Format
                 ("ht {0}, Cells {1}, {2}", this.house, String.Join(",", cells), 
                  String.Join(",", Set.toArray candidates))) |> ignore
        Set.iter (fun (cr : CandidateReduction) -> sb.AppendLine(String.Format("  {0}", cr)) |> ignore) 
            this.candidateReductions

        sb.ToString()

let hiddenTriplesPerHouse (alphabet : Candidate list) (candidateLookup : Cell -> Set<Candidate>) 
    (houseCells : House -> Set<Cell>) (house : House) = 

    let cells = houseCells house

    let candidateCells = Set.map (fun cell -> ((candidateLookup cell), cell)) cells

    let houseCandidates = Set.map fst candidateCells |> Set.unionMany

    let salphabet = List.toSeq alphabet
    
    let hs = 
        Seq.mapi (fun i symbol1 -> 
            Seq.mapi (fun j symbol2 -> 
                Seq.mapi (fun k symbol3 -> 
                    let symbols = set [ symbol1; symbol2; symbol3 ]

                    if Set.isSubset symbols houseCandidates then 
                        let fcc = 
                            Set.map 
                                (fun symbol -> 
                                Set.filter (fun (candidates, _) -> Set.contains symbol candidates) candidateCells) 
                                symbols

                        let a = Set.unionMany fcc

                        if Set.count a = 3 then 

                            let candidateReductions = 
                                Set.map (fun (candidates, cell) -> 
                                    { CandidateReduction.cell = cell
                                      symbols = Set.difference candidates symbols }) a
                            
                            let nonEmptyCandidateReductions = 
                                Set.filter (fun cr -> Set.count cr.symbols > 0) candidateReductions
                            
                            let pointers = 
                                Set.map (fun (candidates, cell) -> 
                                    { CandidateReduction.cell = cell
                                      symbols = Set.intersect candidates symbols }) a

                            if Set.count nonEmptyCandidateReductions > 0 then 
                                Some { HiddenTriple.candidateReductions = nonEmptyCandidateReductions
                                       pointers = pointers
                                       house = house }
                            else None
                        else None
                    else None) (Seq.skip (i + j + 2) salphabet)) (Seq.skip (i + 1) salphabet)) salphabet
    
    let hhs = 
        hs
        |> Seq.concat
        |> Seq.concat
    
    Seq.choose id hhs |> Seq.toList

let hiddenTripleFind (alphabet : Candidate list) (candidateLookup : Cell -> Set<Candidate>) 
    (houseCells : House -> Set<Cell>) (houses : House list) = 
    List.collect (hiddenTriplesPerHouse alphabet candidateLookup houseCells) houses

let hiddenTripleToDescription (hint : HiddenTriple) : HintDescription = 
    { HintDescription.house = Some hint.house
      candidateReductions = hint.candidateReductions
      setCellValue = None
      pointers = hint.pointers }
