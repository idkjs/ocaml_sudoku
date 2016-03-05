module input.load

open core.sudoku
open core.puzzlemap

(* Load a sudoku given as a single line of gridSize*gridSize characters *)
let loadPuzzle (cells : cell array) (alphabetisedLine : digit option array) : lookup<cell, digit option> = 
    Array.zip cells alphabetisedLine
    |> Map.ofArray
    |> mapLookup<cell, digit option>
    :> lookup<cell, digit option>

let load (puzzleShape : puzzleShape) (sudoku : string) : solution = 

    let charToDigit (trialDigit : char) : digit option = 
        let compareAlpha (Digit charDigit) = trialDigit = charDigit
        Array.tryFind compareAlpha puzzleShape.alphabet

    let alphabetisedLine =
        Array.ofSeq sudoku
        |> Array.map charToDigit

    let p = tPuzzleMap puzzleShape :> puzzleMap

    let given = loadPuzzle p.cells alphabetisedLine

    let current = givenToCurrent p.cells given (Digits.ofArray puzzleShape.alphabet)

    { solution.given = given
      current = current
      steps = [] }
