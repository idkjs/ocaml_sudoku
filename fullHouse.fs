module fullHouse

// Full House means:
// For a house there is only one cell that is neither given nor set i.e. has candidates

open System
open System.Text

open sudoku
open puzzlemap
open hints
open console

type FullHouse = {
    cell : Cell
    symbol : Symbol
    house : House
    houseCells : Set<Cell>
}

let fullHousePerHouse (candidateLookup:CandidateLookup) (puzzleMaps:PuzzleMaps) (house:House) =

    let cells = getHouseCells puzzleMaps house

    let candidateCells = cells |> List.map (fun cell -> ((candidateLookup cell), cell))

    let hhs = List.filter (fun (candidates, _) -> Set.isEmpty candidates = false) candidateCells

    if hhs.Length = 1 then
        let h = List.head hhs

        [ {FullHouse.cell = snd h; symbol = first (fst h); house = house; houseCells = Set.ofList cells} ]
    else
        []

let findFullHouse (candidateLookup:CandidateLookup) (puzzleMaps:PuzzleMaps) =

    let perHouse = fullHousePerHouse candidateLookup puzzleMaps

    let houses = allHouses puzzleMaps

    List.collect perHouse houses

let printFullHouse {FullHouse.cell = cell; symbol = symbol; house = house} =
    String.Format ("{0}, Value {1}, Cell {2}", formatHouse house, formatSymbol symbol, formatCell cell)

let fullHouseSymbolTo (hint:FullHouse) (etoc:Entry->FormatLabel) (cell:Cell) =
    if cell = hint.cell then
        konst (LHintCell hint.symbol)
    else if Set.contains cell hint.houseCells then
        LHintHouse
    else
        etoc

let fullHouseFullSymbolTo (hint:FullHouse) (etoc:Symbol->Entry->FormatLabelF) (cell:Cell) candidate (entry:Entry) =
    match entry with
    | Given symbol ->
        if Set.contains cell hint.houseCells then
            FLHintHouseGiven symbol
        else
            etoc candidate entry
    | Set symbol ->
        if Set.contains cell hint.houseCells then
            FLHintHouseSet symbol
        else
            etoc candidate entry
    | Candidates(candidates) ->
        if Set.contains candidate candidates then
            if cell = hint.cell then
                FLHintCandidateReduction candidate
            else
                etoc candidate entry
        else
            etoc candidate entry
