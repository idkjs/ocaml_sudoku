module load

open core.sudoku

// Load a sudoku given as a single line of gridSize*gridSize characters
let loadPuzzle (cells : Cell list) (alphabetisedLine : Digit option list) (cell : Cell) : Digit option = 
    let zs = List.zip alphabetisedLine cells
    List.pick (fun (digitOpt, c) -> 
        if c = cell then Some digitOpt
        else None) zs

let load (alphabet : Digit list) (sudoku : char list) 
    (contentsTransformer : (Cell -> Digit option) -> Cell -> CellContents) : Solution = 
    let charToAlphabet (trialDigit : char) : Digit option = 
        let compareAlpha (Digit charDigit) = trialDigit = charDigit
        List.tryFind compareAlpha alphabet
    
    let alphabetisedLine = List.map charToAlphabet sudoku

    let size = (List.length alphabet) * 1<size>

    let puzzleGrid cell = loadPuzzle (cells size) alphabetisedLine cell

    let solutionGrid = contentsTransformer puzzleGrid
    
    let solution = 
        { given = puzzleGrid
          current = solutionGrid
          steps = [] }

    solution
