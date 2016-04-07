open Sudoku
open Format

let cs (c : char) : consoleString = [CChar c]

(* Some predefined characters - for smaller grid *)
let defaultGridChars : gridChars = 
    { h = cs '─';
      v = 
          { l = cs '│';
            m = cs '│';
            r = cs '│' };
      t = 
          { l = cs '┌';
            m = cs '┬';
            r = cs '┐' };
      m = 
          { l = cs '├';
            m = cs '┼';
            r = cs '┤' };
      b = 
          { l = cs '└';
            m = cs '┴';
            r = cs '┘' };
      n = [NL] }

(* Some predefined characters - for smaller grid *)
let defaultCandidateGridChars : candidateGridChars = 
    { h = cs '═';
      hi = cs '─';
      v = 
          { l = cs '║';
            m = cs '║';
            r = cs '║' };
      vi = cs '│';
      t = 
          { mi = cs '╦';
            x = 
                { l = cs '╔';
                  m = cs '╦';
                  r = cs '╗' } };
      m = 
          { mi = cs '╬';
            x = 
                { l = cs '╠';
                  m = cs '╬';
                  r = cs '╣' } };
      mi = 
          { mi = cs '┼';
            x = 
                { l = cs '╠';
                  m = cs '╬';
                  r = cs '╣' } };
      b = 
          { mi = cs '╧';
            x = 
                { l = cs '╚';
                  m = cs '╩';
                  r = cs '╝' } };
      n = [NL] }

let drawDigitCellContents (firstDigit : digit option) (currentDigit : cellContents) : consoleChar = 
    match firstDigit, currentDigit with
    | Some s, _ -> ColouredString(Digit.to_string s, Blue)
    | None, BigNumber s -> ColouredString(Digit.to_string s, Red)
    | None, PencilMarks _ -> CChar '.'

let drawDigitCellString (firstDigit : digit option) (currentDigit : cellContents) : consoleString =
    [drawDigitCellContents firstDigit currentDigit]

let drawBigNumber (annotation' : Hint.annotation) (s : digit) : consoleChar =
    match annotation' with
    | annotation when annotation.primaryHintHouse && annotation.given.IsSome -> 
        ColouredString(Digit.to_string s, Cyan)
    | annotation when annotation.primaryHintHouse -> 
        ColouredString(Digit.to_string s, Yellow)
    | annotation when annotation.secondaryHintHouse && annotation.given.IsSome -> 
        ColouredString(Digit.to_string s, DarkBlue)
    | annotation when annotation.secondaryHintHouse -> 
        ColouredString(Digit.to_string s, DarkRed)
    | annotation when annotation.given.IsSome -> 
        ColouredString(Digit.to_string s, Blue)
    | _ -> ColouredString(Digit.to_string s, Red)

let drawPencilMarks (annotation' : Hint.annotation) (candidate : digit) (candidates : digits) : consoleChar =
    match annotation' with
    | annotation when annotation.setValue.IsSome && annotation.setValue.Value = candidate -> 
        ColouredString(Digit.to_string candidate, Red)
    | annotation when annotation.setValue.IsSome && Digits.contains candidate candidates -> 
        ColouredString(Digit.to_string candidate, DarkYellow)
    | annotation when annotation.setValueReduction.IsSome && annotation.setValueReduction.Value = candidate && Digits.contains candidate candidates -> 
        ColouredString(Digit.to_string candidate, DarkYellow)
    | annotation when Digits.contains candidate annotation.reductions -> 
        ColouredString(Digit.to_string candidate, DarkYellow)
    | annotation when Digits.contains candidate annotation.pointers -> 
        ColouredString(Digit.to_string candidate, Magenta)
    | annotation when Digits.contains candidate annotation.focus && Digits.contains candidate candidates -> 
        ColouredString(Digit.to_string candidate, Yellow)
    | annotation when annotation.primaryHintHouse -> 
        if Digits.contains candidate candidates then ColouredString(Digit.to_string candidate, DarkGreen)
        else CChar ' '
    | annotation when annotation.secondaryHintHouse -> 
        if Digits.contains candidate candidates then ColouredString(Digit.to_string candidate, Green)
        else CChar ' '
    | _ -> 
        if Digits.contains candidate candidates then CStr(Digit.to_string candidate)
        else CChar ' '

let drawDigitCellContentAnnotations centreCandidate (annotations : (cell * Hint.annotation) list) (cell : cell) (candidate : digit) : consoleChar = 

    let annotation' = Smap.get Cell.comparer annotations cell in
    let isCentre = centreCandidate = candidate in

    match annotation'.current with
    | BigNumber _ when not isCentre -> CChar ' '
    | BigNumber s -> drawBigNumber annotation' s
    | PencilMarks digits -> drawPencilMarks annotation' candidate digits

let drawDigitCellContentAnnotationString (centreCandidate : digit) (annotations : (cell * Hint.annotation) list) (cell : cell) (candidate : digit) : consoleString =
    [drawDigitCellContentAnnotations centreCandidate annotations cell candidate]
