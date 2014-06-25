module hints

open sudoku
open puzzlemap

let first (candidates:Set<Symbol>) = Set.toList candidates |> List.head

type CandidateReduction = {
    cell : Cell
    symbols : Set<Symbol>
}
