module load

open core.sudoku
open core.puzzlemap

(* Load a sudoku given as a single line of gridSize*gridSize characters *)
let loadPuzzle (cells : cell array) (alphabetisedLine : digit option array) : lookup<cell, digit option> = 
    Array.zip cells alphabetisedLine
    |> Map.ofArray
    |> mapLookup<cell, digit option>
    :> lookup<cell, digit option>

let load (orderedCells : cell array) (alphabet : digit array) (sudoku : char array) 
    (contentsTransformer : given -> current) : solution = 

    let charToDigit (trialDigit : char) : digit option = 
        let compareAlpha (Digit charDigit) = trialDigit = charDigit
        Array.tryFind compareAlpha alphabet

    let alphabetisedLine = Array.map charToDigit sudoku

    let given = loadPuzzle orderedCells alphabetisedLine

    let current = contentsTransformer given

    { solution.given = given
      current = current
      steps = [] }
