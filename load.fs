module load

open core.sudoku
open core.puzzlemap

// Load a sudoku given as a single line of gridSize*gridSize characters
let loadPuzzle (cells : Cell list) (alphabetisedLine : Digit option list) : Map<Cell, Digit option> = 
    List.zip cells alphabetisedLine
    |> Map.ofList

let load (alphabet : Digit list) (sudoku : char list) 
    (contentsTransformer : Given -> Current) : Solution = 
    let charToAlphabet (trialDigit : char) : Digit option = 
        let compareAlpha (Digit charDigit) = trialDigit = charDigit
        List.tryFind compareAlpha alphabet
    
    let alphabetisedLine = List.map charToAlphabet sudoku

    let size = (List.length alphabet) * 1<size>

    let puzzleGrid = loadPuzzle (orderedCells size) alphabetisedLine

    let given = puzzleGrid

    let solutionGrid = contentsTransformer puzzleGrid
    
    let solution = 
        { given = given
          current = solutionGrid
          steps = [] }

    solution
