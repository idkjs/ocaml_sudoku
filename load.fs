module load

open core.sudoku
open core.puzzlemap

// Load a sudoku given as a single line of gridSize*gridSize characters
let loadPuzzle (cells : cell list) (alphabetisedLine : digit option list) : Map<cell, digit option> = 
    List.zip cells alphabetisedLine
    |> Map.ofList

let load (orderedCells : cell list) (alphabet : digit list) (sudoku : char list) 
    (contentsTransformer : given -> current) : solution = 
    let charToAlphabet (trialDigit : char) : digit option = 
        let compareAlpha (Digit charDigit) = trialDigit = charDigit
        List.tryFind compareAlpha alphabet
    
    let alphabetisedLine = List.map charToAlphabet sudoku

    let puzzleGrid = loadPuzzle orderedCells alphabetisedLine

    let given = puzzleGrid

    let solutionGrid = contentsTransformer puzzleGrid
    
    let solution = 
        { given = given
          current = solutionGrid
          steps = [] }

    solution
