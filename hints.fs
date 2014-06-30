module hints

open sudoku
open puzzlemap

let first (candidates:Set<Candidate>) = Set.toList candidates |> List.head

type CandidateReduction = {
    cell : Cell
    symbols : Set<Candidate>
}
