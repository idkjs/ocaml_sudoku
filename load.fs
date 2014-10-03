module load

open core.sudoku

let charToAlphabet (alphabet : Symbol list) (trialSymbol : char) : Symbol option = 
    let compareAlpha (Symbol charSymbol) = trialSymbol = charSymbol
    List.tryFind compareAlpha alphabet

let loadLine (line : string) (alphabet : Symbol list) : Symbol option list = 
    List.map (charToAlphabet alphabet) (List.ofSeq line)

// Load a sudoku given as a single line of gridSize*gridSize characters
let loadPuzzle (cells : Cell list) (alphabetisedLine : Symbol option list) (cell : Cell) : Symbol option = 
    let zs = List.zip alphabetisedLine cells
    List.pick (fun (symbolOpt, c) -> 
        if c = cell then Some symbolOpt
        else None) zs

let load (alphabet : Symbol list) (sudoku : string) (cell : Cell) : Symbol option = 
    let alphabetisedLine = loadLine sudoku alphabet
    let size = (List.length alphabet) * 1<size>

    loadPuzzle (cells size) alphabetisedLine cell
