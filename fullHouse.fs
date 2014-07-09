module fullHouse

// Full House means:
// For a house there is only one cell that is neither given nor set i.e. has candidates

open System
open System.Text

open sudoku
open puzzlemap
open hints
open console

let fullHousePerHouse (candidateLookup:Cell->Set<Candidate>) (puzzleMaps:PuzzleMaps) (house:House) =

    let cells = getHouseCells puzzleMaps house

    let candidateCells = cells |> List.map (fun cell -> ((candidateLookup cell), cell))

    let hhs = List.filter (fun (candidates, _) -> Set.isEmpty candidates = false) candidateCells

    if hhs.Length = 1 then
        let h = List.head hhs

        [ {FullHouse.cell = snd h; candidate = first (fst h); house = house} ]
    else
        []

let findFullHouse (candidateLookup:Cell->Set<Candidate>) (puzzleMaps:PuzzleMaps) =

    let perHouse = fullHousePerHouse candidateLookup puzzleMaps

    let houses = allHouses puzzleMaps

    List.collect perHouse houses

let fullHouseToString (hint:FullHouse) =
    String.Format ("{0}, Value {1}, Cell {2}", formatHouse hint.house, formatCandidate hint.candidate, formatCell hint.cell)

let fullHouseSymbolTo (hint:FullHouse) (puzzleMaps:PuzzleMaps) : (Cell->AnnotatedSymbol)->(Cell->HintAnnotatedSymbol) =
    let houseCells = getHouseCells puzzleMaps hint.house |> Set.ofList

    fun (etoc:Cell->AnnotatedSymbol) ->
        fun (cell:Cell) ->
            let entry = etoc cell
            if cell = hint.cell then
                HASCell hint.candidate
            else if Set.contains cell houseCells then
                HASHouse entry
            else
                HASId entry
