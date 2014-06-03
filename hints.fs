#light

module hints

//open System
//open System.Text

open sudoku
open tactics

type CandidateLookup = Cell -> Set<Symbol>

// Hidden Single means:
// For a house and a symbol: there is only one cell in the house with this symbol as a candidate
type HiddenSingle = {
    cell : Cell
    symbol : Symbol
    house : House
}

//getCandidateEntries (entryLookup
let hiddenSinglesPerHouse alphabet (candidateLookup:CandidateLookup) (houseCells:HouseCells) (house:House) =

    let cells = getHouseCells houseCells house

    let candidateCells = cells |> List.map (fun cell -> ((candidateLookup cell), cell))

    let hs =
        List.map (
            fun symbol ->
                let filteredCandidateCells = List.filter (fun (candidates, _) -> Set.contains symbol candidates) candidateCells
                let cells = List.map snd filteredCandidateCells
                (filteredCandidateCells, symbol))
            alphabet

    let hhs = List.filter (fun (filteredCandidateCells, _) -> List.length filteredCandidateCells = 1) hs

    let hhhs =
        List.map (
            fun (filteredCandidateCells, symbol) -> {cell = List.head filteredCandidateCells |> snd; symbol = symbol; house = house})
            hhs

    hhhs

let allHouses (columns:Column list) (rows:Row list) (boxes:Box list) =
    let chs =
        List.map (
            fun c -> Column c)
            columns

    let rhs =
        List.map (
            fun r -> Row r)
            rows

    let bhs =
        List.map (
            fun b -> Box b)
            boxes

    List.concat [ chs; rhs; bhs ]

// Hidden singles is when a house has only one possible square for a candidate
let findHiddenSingles alphabet (entryLookup:EntryLookup) (columns:Column list) (rows:Row list) (boxes:Box list) (houseCells:HouseCells) =

    let candidateLookup = entryLookup >> getCandidateEntries

    let perHouse = hiddenSinglesPerHouse alphabet candidateLookup houseCells

    let houses = allHouses columns rows boxes

    List.collect perHouse houses


// Naked Single means:
// For a cell there is only one candidate
type NakedSingle = {
    cell : Cell
    symbol : Symbol
}

let first (candidates:Set<Symbol>) = Set.toList candidates |> List.head

let findNakedSingles (entryLookup:EntryLookup) (cells:Cell list) =

    let candidateLookup = entryLookup >> getCandidateEntries

    let candidateCells =
        List.map (fun cell -> (candidateLookup cell, cell)) cells

    let filteredCandidateCells = List.filter (fun (candidates, _) -> Set.count candidates = 1) candidateCells


    List.map (fun (candidates, cell) -> {NakedSingle.cell=cell; NakedSingle.symbol=first candidates }) filteredCandidateCells


// Full House means:
// For a house there is only one cell that is neither given nor set i.e. has candidates
type FullHouse = {
    cell : Cell
    symbol : Symbol
    house : House
}

let fullHousePerHouse (candidateLookup:CandidateLookup) (houseCells:HouseCells) (house:House) =

    let cells = getHouseCells houseCells house

    let candidateCells = cells |> List.map (fun cell -> ((candidateLookup cell), cell))

    let hhs = List.filter (fun (candidates, _) -> Set.isEmpty candidates = false) candidateCells

    if hhs.Length = 1 then
        let h = List.head hhs

        [ {FullHouse.cell = snd h; symbol = first (fst h); house = house} ]
    else
        []

let findFullHouse (entryLookup:EntryLookup) (columns:Column list) (rows:Row list) (boxes:Box list) (houseCells:HouseCells) =

    let candidateLookup = entryLookup >> getCandidateEntries

    let perHouse = fullHousePerHouse candidateLookup houseCells

    let houses = allHouses columns rows boxes

    List.collect perHouse houses


type CandidateReduction = {
    cell : Cell
    symbols : Set<Symbol>
}

type NakedPair = {
    cell1 : Cell
    cell2 : Cell
    symbols : Set<Symbol>
    candidateReduction : CandidateReduction list
    house : House
}

let nakedPairPerHouse (candidateLookup:CandidateLookup) (houseCells:HouseCells) (house:House) =

    let cells = getHouseCells houseCells house

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
                            fun {symbols = candidates3; cell = cell3} ->
                                cell <> cell3 && cell2 <> cell3 && Set.count candidates3 > 0)
                            reductionCandidates

                    if reductions.Length > 0 then
                        let hint = { NakedPair.cell1 = cell; cell2 = cell2; symbols = candidates; candidateReduction = reductions; house = house }
                        hints := hint :: !hints
                ) (Seq.skip (i + 1) hht)) hht
    !hints

let findNakedPairs (entryLookup:EntryLookup) (columns:Column list) (rows:Row list) (boxes:Box list) (houseCells:HouseCells) =

    let candidateLookup = entryLookup >> getCandidateEntries

    let perHouse = nakedPairPerHouse candidateLookup houseCells

    let houses = allHouses columns rows boxes

    List.collect perHouse houses

