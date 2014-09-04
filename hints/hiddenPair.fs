module hints.hiddenPair

// Hidden Pair means:
// For a house and a symbol: there is only one cell in the house with this symbol as a candidate
open System
open System.Text

open console

open core.setCell
open core.sudoku
open hints

type HiddenPair = 
    { candidateReductions : Set<CandidateReduction>
      pointers : Set<CandidateReduction>
      house : House }
    override this.ToString() = 
        let sb = StringBuilder()

        let cells = Set.toArray (Set.map (fun p -> p.cell) this.pointers)
        let candidates = Set.unionMany (Set.toArray (Set.map (fun p -> p.symbols) this.pointers))

        sb.AppendLine
            (String.Format
                 ("hp {0}, Cells {1}, {2}", this.house, String.Join(",", cells), 
                  String.Join(",", Set.toArray candidates))) |> ignore
        Set.iter (fun (cr : CandidateReduction) -> sb.AppendLine(String.Format("  {0}", cr)) |> ignore) 
            this.candidateReductions

        sb.ToString()

let hiddenPairsPerHouse (alphabet : Candidate list) (candidateLookup : Cell -> Set<Candidate>) 
    (houseCells : House -> Set<Cell>) (house : House) = 

    let cells = houseCells house

    let candidateCells = Set.map (fun cell -> ((candidateLookup cell), cell)) cells

    let salphabet = List.toSeq alphabet
    
    let hs = 
        Seq.mapi 
            (fun i symbol1 -> 
            Seq.mapi (fun j symbol2 -> 
                let filteredCandidateCells = 
                    Set.filter 
                        (fun (candidates, _) -> Set.contains symbol1 candidates && Set.contains symbol2 candidates) 
                        candidateCells
                let filteredCandidateCells2 = 
                    Set.filter (fun (candidates, _) -> Set.contains symbol1 candidates) candidateCells
                let filteredCandidateCells3 = 
                    Set.filter (fun (candidates, _) -> Set.contains symbol2 candidates) candidateCells

                (filteredCandidateCells, filteredCandidateCells2, filteredCandidateCells3, symbol1, symbol2)) 
                (Seq.skip (i + 1) salphabet)) salphabet
    
    let hhs = Seq.concat hs |> Seq.toList

    let hhs2 = 
        List.filter 
            (fun (filteredCandidateCells, filteredCandidateCells2, filteredCandidateCells3, _, _) -> 
            Set.count filteredCandidateCells = 2 && Set.count filteredCandidateCells2 = 2 
            && Set.count filteredCandidateCells3 = 2) hhs
    
    let hhs3 = 
        List.map (fun (candidateCells, _, _, candidate1, candidate2) -> 
            let s = 
                Set.map (fun (candidates, cell) -> (Set.difference candidates (set [ candidate1; candidate2 ]), cell)) 
                    candidateCells
            //let u = Set.unionMany (Set.toSeq s)
            let u = Set.filter (fun (candidates, _) -> Set.count candidates > 0) s
            
            let t = 
                Set.map (fun (candidates, cell) -> 
                    { CandidateReduction.cell = cell
                      symbols = candidates }) u
            (candidateCells, candidate1, candidate2, t)) hhs2
    
    let hhs4 = List.filter (fun (filteredCandidateCells, candidate1, candidate2, crs) -> Set.count crs > 0) hhs3
    
    let hhhs = 
        List.map (fun (filteredCandidateCells, candidate1, candidate2, crs) -> 
            { HiddenPair.candidateReductions = crs
              pointers = 
                  Set.map (fun (candidates, cell) -> 
                      { CandidateReduction.cell = cell
                        symbols = set [ candidate1; candidate2 ] }) filteredCandidateCells
              house = house }) hhs4

    hhhs

let hiddenPairFind (alphabet : Candidate list) (candidateLookup : Cell -> Set<Candidate>) 
    (houseCells : House -> Set<Cell>) (houses : House list) = 
    List.collect (hiddenPairsPerHouse alphabet candidateLookup houseCells) houses

let hiddenPairToDescription (hint : HiddenPair) : HintDescription = 
    { HintDescription.house = Some hint.house
      candidateReductions = hint.candidateReductions
      setCellValue = None
      pointers = hint.pointers }
