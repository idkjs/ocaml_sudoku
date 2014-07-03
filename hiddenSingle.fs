﻿module hiddenSingle

// Hidden Single means:
// For a house and a symbol: there is only one cell in the house with this symbol as a candidate

open System
open System.Text

open sudoku
open puzzlemap
open console

type HiddenSingle = {
    cell : Cell
    symbol : Candidate
    house : House
    houseCells : Set<Cell>
}

let hiddenSinglesPerHouse (alphabet:Candidate list) (candidateLookup:Cell->Set<Candidate>) (puzzleMaps:PuzzleMaps) (house:House) =

    let cells = getHouseCells puzzleMaps house

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
            fun (filteredCandidateCells, symbol) -> {cell = List.head filteredCandidateCells |> snd; symbol = symbol; house = house; houseCells = Set.ofList cells})
            hhs

    hhhs


let findHiddenSingles (alphabet:Candidate list) (candidateLookup:Cell->Set<Candidate>) (puzzleMaps:PuzzleMaps) =

    let perHouse = hiddenSinglesPerHouse alphabet candidateLookup puzzleMaps

    let houses = allHouses puzzleMaps

    List.collect perHouse houses

let formatHiddenSingle {HiddenSingle.cell = cell; symbol = symbol; house = house} =
    String.Format ("hs {0}, Value {1}, Cell {2}", formatHouse house, formatCandidate symbol, formatCell cell)

let hiddenSingleSymbolTo (hint:HiddenSingle) : (Cell->AnnotatedSymbol)->(Cell->HintAnnotatedSymbol) =
    fun (etoc:Cell->AnnotatedSymbol) ->
        fun cell ->
            if cell = hint.cell then
                HASCell (candidateToSymbol hint.symbol)
            else if Set.contains cell hint.houseCells then
                HASHouse (etoc cell)
            else
                HASId (etoc cell)