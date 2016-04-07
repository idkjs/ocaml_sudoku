open Sudoku
open Format
open Hint
(*F# open FSharp.Compatibility.OCaml F#*)

let drawDigitCellContents (firstDigit : digit option) (currentDigit : cellContents) : consoleChar = 
    match firstDigit, currentDigit with
    | Some s, _ -> ColouredString(Digit.to_string s, Blue)
    | None, BigNumber s -> ColouredString(Digit.to_string s, Red)
    | None, PencilMarks _ -> CChar '.'

let drawDigitCellString (firstDigit : digit option) (currentDigit : cellContents) : consoleString =
    [drawDigitCellContents firstDigit currentDigit]

let drawBigNumber (annotation : Hint.annotation) (s : digit) : consoleChar =
    if annotation.primaryHintHouse then
        match annotation.given with
        | Some _ -> ColouredString(Digit.to_string s, Cyan)
        | None -> ColouredString(Digit.to_string s, Yellow)
    else if annotation.secondaryHintHouse then
        match annotation.given with
        | Some _ -> ColouredString(Digit.to_string s, DarkBlue)
        | None -> ColouredString(Digit.to_string s, DarkRed)
    else
        match annotation.given with
        | Some _ -> ColouredString(Digit.to_string s, Blue)
        | None -> ColouredString(Digit.to_string s, Red)

let drawPencilMarks (annotation : Hint.annotation) (candidate : digit) (candidates : digits) : consoleChar =
    match annotation.setValue with
    | Some vv when vv = candidate -> 
        ColouredString(Digit.to_string candidate, Red)
    | Some _ when Digits.contains candidate candidates -> 
        ColouredString(Digit.to_string candidate, DarkYellow)
    | _ ->
        (match annotation.setValueReduction with
         | Some svr when svr = candidate && Digits.contains candidate candidates -> 
            ColouredString(Digit.to_string candidate, DarkYellow)
         | _ ->
            (if Digits.contains candidate annotation.reductions then
                ColouredString(Digit.to_string candidate, DarkYellow)
             else if Digits.contains candidate annotation.pointers then
                ColouredString(Digit.to_string candidate, Magenta)
             else if Digits.contains candidate annotation.focus && Digits.contains candidate candidates then
                ColouredString(Digit.to_string candidate, Yellow)
             else if annotation.primaryHintHouse then
                if Digits.contains candidate candidates then ColouredString(Digit.to_string candidate, DarkGreen)
                else CChar ' '
             else if annotation.secondaryHintHouse then
                if Digits.contains candidate candidates then ColouredString(Digit.to_string candidate, Green)
                else CChar ' '
             else
                if Digits.contains candidate candidates then CStr(Digit.to_string candidate)
                else CChar ' '))

let drawDigitCellContentAnnotations centreCandidate (annotations : (cell * Hint.annotation) list) (cell : cell) (candidate : digit) : consoleChar = 

    let annotation' = Smap.get Cell.comparer annotations cell in
    let isCentre = centreCandidate = candidate in

    match annotation'.current with
    | BigNumber _ when not isCentre -> CChar ' '
    | BigNumber s -> drawBigNumber annotation' s
    | PencilMarks digits -> drawPencilMarks annotation' candidate digits

let drawDigitCellContentAnnotationString (centreCandidate : digit) (annotations : (cell * Hint.annotation) list) (cell : cell) (candidate : digit) : consoleString =
    [drawDigitCellContentAnnotations centreCandidate annotations cell candidate]
