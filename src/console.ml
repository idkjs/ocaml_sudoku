open Sudoku
open Format
open Hint
(*F# open FSharp.Compatibility.OCaml F#*)

let drawDigitCellContents (given : digit option) (current : cellContents) : consoleChar = 
    match given, current with
    | Some s, _ -> ColouredDigit(s, Blue, DefaultColour)
    | None, BigNumber s -> ColouredDigit(s, Red, DefaultColour)
    | None, PencilMarks _ -> CChar '.'

let drawDigitCellString (given : digit option) (current : cellContents) : consoleString =
    [drawDigitCellContents given current]

let drawBigNumber (annotation : annotation) (digit : digit) : consoleChar =
    if annotation.primaryHintHouse then
        match annotation.given with
        | Some _ -> ColouredDigit(digit, Cyan, DefaultColour)
        | None -> ColouredDigit(digit, Yellow, DefaultColour)
    else if annotation.secondaryHintHouse then
        match annotation.given with
        | Some _ -> ColouredDigit(digit, DefaultColour, Blue)
        | None -> ColouredDigit(digit, DefaultColour, Red)
    else
        match annotation.given with
        | Some _ -> ColouredDigit(digit, Blue, White)
        | None -> ColouredDigit(digit, Red, DefaultColour)

let drawPencilMarks (annotation : Hint.annotation) (candidate : digit) (candidates : digits) : consoleChar =
    match annotation.setValue with
    | Some vv when vv = candidate -> 
        ColouredDigit(candidate, Red, DefaultColour)
    | Some _ when Digits.contains candidate candidates -> 
        ColouredDigit(candidate, Green, DefaultColour)
    | _ ->
        (match annotation.setValueReduction with
         | Some svr when svr = candidate && Digits.contains candidate candidates -> 
            ColouredDigit(candidate, Green, DefaultColour)
         | _ ->
            (if Digits.contains candidate annotation.reductions then
                ColouredDigit(candidate, Green, DefaultColour)
             else if Digits.contains candidate annotation.pointers then
                ColouredDigit(candidate, Magenta, DefaultColour)
             else if Digits.contains candidate annotation.focus && Digits.contains candidate candidates then
                ColouredDigit(candidate, Yellow, DefaultColour)
             else if annotation.primaryHintHouse then
                if Digits.contains candidate candidates then ColouredDigit(candidate, Cyan, DefaultColour)
                else CChar ' '
             else if annotation.secondaryHintHouse then
                if Digits.contains candidate candidates then ColouredDigit(candidate, Green, DefaultColour)
                else CChar ' '
             else
                if Digits.contains candidate candidates then ColouredDigit(candidate, Green, DefaultColour)
                else CChar ' '))

let drawDigitCellContentAnnotations centreCandidate (annotations : (cell * Hint.annotation) list) (cell : cell) (candidate : digit) : consoleChar = 

    let annotation = Smap.get Cell.comparer cell annotations in

    match annotation.current with
    | BigNumber s when centreCandidate = candidate -> drawBigNumber annotation s
    | BigNumber _ -> ColouredString(" ", Blue, White)
    | PencilMarks digits -> drawPencilMarks annotation candidate digits

let drawDigitCellContentAnnotationString (centreCandidate : digit) (annotations : (cell * Hint.annotation) list) (cell : cell) (candidate : digit) : consoleString =
    [drawDigitCellContentAnnotations centreCandidate annotations cell candidate]
