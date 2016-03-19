module input.load

open core.smap
open core.sudoku
open core.puzzlemap

(* Load a sudoku given as a single line of gridSize*gridSize characters *)
let loadPuzzle (cells : cell list) (alphabetisedLine : digit option list) : SMap<cell, digit option> = 
    List.zip cells alphabetisedLine
    |> SMap.ofList

let load (puzzleShape : puzzleShape) (sudoku : string) : solution = 

    let charToDigit (trialDigit : char) : digit option = 
        let compareAlpha (Digit charDigit) = trialDigit = charDigit
        List.tryFind compareAlpha puzzleShape.alphabet

    let alphabetisedLine =
        sudoku
        |> List.ofSeq
        |> List.map charToDigit

    let p = tPuzzleMap puzzleShape

    let given = loadPuzzle p.cells alphabetisedLine

    let current = givenToCurrent p.cells given (Digits.ofList puzzleShape.alphabet)

    { solution.given = given
      current = current
      steps = [ Load sudoku ] }
