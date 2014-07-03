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
    symbol : Candidate
    house : House
    houseCells : Set<Cell>
}

let fullHousePerHouse (candidateLookup:Cell->Set<Candidate>) (puzzleMaps:PuzzleMaps) (house:House) =

    let cells = getHouseCells puzzleMaps house

    let candidateCells = cells |> List.map (fun cell -> ((candidateLookup cell), cell))

    let hhs = List.filter (fun (candidates, _) -> Set.isEmpty candidates = false) candidateCells

    if hhs.Length = 1 then
        let h = List.head hhs

        [ {FullHouse.cell = snd h; symbol = first (fst h); house = house; houseCells = Set.ofList cells} ]
    else
        []

let findFullHouse (candidateLookup:Cell->Set<Candidate>) (puzzleMaps:PuzzleMaps) =

    let perHouse = fullHousePerHouse candidateLookup puzzleMaps

    let houses = allHouses puzzleMaps

    List.collect perHouse houses

let printFullHouse (hint:FullHouse) =
    String.Format ("{0}, Value {1}, Cell {2}", formatHouse hint.house, formatCandidate hint.symbol, formatCell hint.cell)

let fullHouseSymbolTo (hint:FullHouse) : (Cell->AnnotatedSymbol)->(Cell->HintAnnotatedSymbol) =
    fun (etoc:Cell->AnnotatedSymbol) ->
        fun (cell:Cell) ->
            let entry = etoc cell
            if cell = hint.cell then
                HASCell (candidateToSymbol hint.symbol)
            else if Set.contains cell hint.houseCells then
                HASHouse entry
            else
                HASId entry

let fullHouseFullSymbolTo (hint:FullHouse) : (Cell->Candidate->AnnotatedSymbol)->(Cell->Candidate->FormatLabelF) =
    fun (etoc:Cell->Candidate->AnnotatedSymbol) ->
        fun (cell:Cell) (candidate:Candidate) ->
            let label = etoc cell candidate

            if cell = hint.cell then
                FLHintCell (candidateToSymbol hint.symbol)
            else if Set.contains cell hint.houseCells then
                FLHintHouse label
            else
                FLPlain label

