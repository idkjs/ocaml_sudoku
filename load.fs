module load

open core.sudoku

// Load a sudoku given as a single line of gridSize*gridSize characters
let loadPuzzle (cells : Cell list) (alphabetisedLine : Symbol option list) (cell : Cell) : Symbol option = 
    let zs = List.zip alphabetisedLine cells
    List.pick (fun (symbolOpt, c) -> 
        if c = cell then Some symbolOpt
        else None) zs

let load (alphabet : Symbol list) (sudoku : char list) 
    (contentsTransformer : (Cell -> Symbol option) -> Cell -> CellContents) : Solution = 
    let charToAlphabet (trialSymbol : char) : Symbol option = 
        let compareAlpha (Symbol charSymbol) = trialSymbol = charSymbol
        List.tryFind compareAlpha alphabet
    
    let alphabetisedLine = List.map charToAlphabet sudoku

    let size = (List.length alphabet) * 1<size>

    let puzzleGrid cell = loadPuzzle (cells size) alphabetisedLine cell

    let solutionGrid = contentsTransformer puzzleGrid
    
    let solution = 
        { start = puzzleGrid
          current = solutionGrid
          steps = [] }

    solution
