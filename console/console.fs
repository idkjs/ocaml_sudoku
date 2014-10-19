module console.console

open System
open System.Drawing
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

open core.sudoku

open format

[<StructLayout(LayoutKind.Sequential)>]
type COORD =
    val mutable X : int16
    val mutable Y : int16
    new(x, y) = { X = x; Y = y }

[<StructLayout(LayoutKind.Sequential)>]
type SMALL_RECT =
  val mutable Left : int16
  val mutable Top : int16
  val mutable Right : int16
  val mutable Bottom : int16
  new(l, t, r, b) = { Left = l; Top = t; Right = r; Bottom = b }

[<StructLayout(LayoutKind.Sequential)>]
type COLORREF =
    val mutable ColorDWORD : uint32
    new(color : Color) = { ColorDWORD = (uint32)color.R + (((uint32)color.G) <<< 8) + (((uint32)color.B) <<< 16) }
    new(r : uint32, g : uint32, b : uint32) = { ColorDWORD = r + (g <<< 8) + (b <<< 16) }
    member x.GetColor() : Color = 
        Color.FromArgb((int)(0x000000FFul &&& x.ColorDWORD), (int)(0x0000FF00ul &&& x.ColorDWORD) >>> 8, (int)(0x00FF0000ul &&& x.ColorDWORD) >>> 16)
    member x.SetColor (color : Color) : unit =
        x.ColorDWORD <- (uint32)color.R + (((uint32)color.G) <<< 8) + (((uint32)color.B) <<< 16)

[<StructLayout(LayoutKind.Sequential)>]
type CONSOLE_SCREEN_BUFFER_INFO_EX =
    val mutable cbSize : uint32
    val mutable dwSize : COORD
    val mutable dwCursorPosition : COORD
    val mutable wAttributes : uint16
    val mutable srWindow : SMALL_RECT
    val mutable dwMaximumWindowSize : COORD
    val mutable wPopupAttributes : uint16
    val mutable bFullscreenSupported : bool
    val mutable col0 : COLORREF
    val mutable col1 : COLORREF
    val mutable col2 : COLORREF
    val mutable col3 : COLORREF
    val mutable col4 : COLORREF
    val mutable col5 : COLORREF
    val mutable col6 : COLORREF
    val mutable col7 : COLORREF
    val mutable col8 : COLORREF
    val mutable col9 : COLORREF
    val mutable col10 : COLORREF
    val mutable col11 : COLORREF
    val mutable col12 : COLORREF
    val mutable col13 : COLORREF
    val mutable col14 : COLORREF
    val mutable col15 : COLORREF
    new(c) =
        {
            cbSize = 96ul
            dwSize = COORD(0s, 0s); dwCursorPosition = COORD(0s, 0s); wAttributes = c; srWindow = SMALL_RECT(0s, 0s, 0s, 0s); dwMaximumWindowSize = COORD(0s, 0s)
            wPopupAttributes = 0us
            bFullscreenSupported = false
            col0 = COLORREF(0ul, 0ul, 0ul)
            col1  = COLORREF(0ul, 0ul, 0ul)
            col2  = COLORREF(0ul, 0ul, 0ul)
            col3  = COLORREF(0ul, 0ul, 0ul)
            col4  = COLORREF(0ul, 0ul, 0ul)
            col5  = COLORREF(0ul, 0ul, 0ul)
            col6  = COLORREF(0ul, 0ul, 0ul)
            col7  = COLORREF(0ul, 0ul, 0ul)
            col8  = COLORREF(0ul, 0ul, 0ul)
            col9  = COLORREF(0ul, 0ul, 0ul)
            col10 = COLORREF(0ul, 0ul, 0ul)
            col11 = COLORREF(0ul, 0ul, 0ul)
            col12 = COLORREF(0ul, 0ul, 0ul)
            col13 = COLORREF(0ul, 0ul, 0ul)
            col14 = COLORREF(0ul, 0ul, 0ul)
            col15 = COLORREF(0ul, 0ul, 0ul)
        }

[<DllImport("kernel32.dll", SetLastError = true)>]
extern IntPtr GetStdHandle(int32 nStdHandle)

[<DllImport("kernel32.dll", SetLastError = true)>]
extern bool GetConsoleScreenBufferInfoEx(IntPtr, [<In>][<Out>]CONSOLE_SCREEN_BUFFER_INFO_EX b)

//[<DllImport("kernel32.dll")>]
//extern bool SetConsoleTextAttribute(IntPtr hConsoleOutput, int16 wAttributes)

[<DllImport("kernel32.dll")>]
extern int32 GetLastError()

[<DllImport("kernel32.dll", SetLastError = true)>]
extern bool SetConsoleScreenBufferInfoEx(IntPtr ConsoleOutput, CONSOLE_SCREEN_BUFFER_INFO_EX ConsoleScreenBufferInfoEx);

// Things we may want to write
type ConsoleChar = 
    | CChar of char
    | CStr of string
    | ColouredChar of char * ConsoleColor
    | ColouredString of string * ConsoleColor
    | NL

let cs = CChar >> Seq.singleton

// Some predefined characters - for smaller grid
let defaultGridChars : gridChars<seq<ConsoleChar>> = 
    { h = cs '─'
      v = 
          { l = cs '│'
            m = cs '│'
            r = cs '│' }
      t = 
          { l = cs '┌'
            m = cs '┬'
            r = cs '┐' }
      m = 
          { l = cs '├'
            m = cs '┼'
            r = cs '┤' }
      b = 
          { l = cs '└'
            m = cs '┴'
            r = cs '┘' }
      n = Seq.singleton NL }

// Some predefined characters - for smaller grid
let defaultCandidateGridChars : candidateGridChars<seq<ConsoleChar>> = 
    { h = cs '═'
      hi = cs '─'
      v = 
          { l = cs '║'
            m = cs '║'
            r = cs '║' }
      vi = cs '│'
      t = 
          { mi = cs '╦'
            x = 
                { l = cs '╔'
                  m = cs '╦'
                  r = cs '╗' } }
      m = 
          { mi = cs '╬'
            x = 
                { l = cs '╠'
                  m = cs '╬'
                  r = cs '╣' } }
      mi = 
          { mi = cs '┼'
            x = 
                { l = cs '╠'
                  m = cs '╬'
                  r = cs '╣' } }
      b = 
          { mi = cs '╧'
            x = 
                { l = cs '╚'
                  m = cs '╩'
                  r = cs '╝' } }
      n = Seq.singleton NL }

// Change the console colour to write a string
let consoleWriteColor (value : 'a) consoleColour = 
    let foregroundColour = System.Console.ForegroundColor
    try 
        System.Console.ForegroundColor <- consoleColour
        System.Console.Write value
    finally
        System.Console.ForegroundColor <- foregroundColour

let consoleWriteColor2 (value : 'a) consoleColour = 
    let mapColourToRGB r g b = r + (g <<< 8) + (b <<< 16)

    //let mapColourToRGB' (c : ConsoleColor) = mapColourToRGB c.R c.G c.B

    let mutable bufferInfo = CONSOLE_SCREEN_BUFFER_INFO_EX(0us)
    let consoleHandle = GetStdHandle(-11)

    let b0 = GetConsoleScreenBufferInfoEx(consoleHandle, bufferInfo)
    let le0 = GetLastError()

    //SetConsoleTextAttribute(consoleHandle, 8s + 2s) |> ignore
    bufferInfo.srWindow.Bottom <- bufferInfo.srWindow.Bottom + 1s
    bufferInfo.srWindow.Right <- bufferInfo.srWindow.Right + 1s

    bufferInfo.col7 <- COLORREF(0xFFul, 0ul, 0ul)

    (*
    bufferInfo.col0 <- mapColourToRGB 255ul 0ul 0ul
    bufferInfo.col1 <- mapColourToRGB 255ul 0ul 0ul
    bufferInfo.col2 <- mapColourToRGB 255ul 0ul 0ul
    bufferInfo.col3 <- mapColourToRGB 255ul 0ul 0ul
    bufferInfo.col4 <- mapColourToRGB 255ul 0ul 0ul
    bufferInfo.col5 <- mapColourToRGB 255ul 0ul 0ul
    bufferInfo.col6 <- mapColourToRGB 255ul 0ul 0ul
    bufferInfo.col7 <- mapColourToRGB 255ul 0ul 0ul
    bufferInfo.col8 <- mapColourToRGB 255ul 0ul 0ul
    bufferInfo.col9 <- mapColourToRGB 255ul 0ul 0ul
    bufferInfo.col10 <- mapColourToRGB 255ul 0ul 0ul
    bufferInfo.col11 <- mapColourToRGB 255ul 0ul 0ul
    bufferInfo.col12 <- mapColourToRGB 255ul 0ul 0ul
    bufferInfo.col13 <- mapColourToRGB 255ul 0ul 0ul
    bufferInfo.col14 <- mapColourToRGB 255ul 0ul 0ul
    bufferInfo.col15 <- mapColourToRGB 255ul 0ul 0ul
    *)
    let b1 = SetConsoleScreenBufferInfoEx(consoleHandle, bufferInfo)
    let le1 = GetLastError()
    System.Console.Write value
    //SetConsoleTextAttribute(consoleHandle, bufferInfo.wAttributes) |> ignore

let drawConsoleChar (consoleChar : ConsoleChar) = 
    match consoleChar with
    | CChar c -> Console.Write c
    | CStr c -> Console.Write c
    | ColouredChar(c, consoleColour) -> consoleWriteColor c consoleColour
    | ColouredString(c, consoleColour) -> consoleWriteColor c consoleColour
    | NL -> Console.WriteLine ""

let drawDigitCellContents (firstDigit : Digit option) (currentDigit : CellContents) = 
    match firstDigit, currentDigit with
    | Some s, _ -> ColouredString(s.ToString(), ConsoleColor.Blue)
    | None, BigNumber s -> ColouredString(s.ToString(), ConsoleColor.Red)
    | None, PencilMarks _ -> CChar '.'

let drawDigitCellContentAnnotations centreCandidate (focusDigit : Digit option) (candidate : Digit) (firstDigit : Digit option) 
    (currentDigit : CellContents) (currentHintDigitOpt : CellAnnotation option) = 
    let isCentre = centreCandidate = candidate

    match firstDigit, currentDigit with
    | Some _, _ when not isCentre -> CChar ' '
    | Some s, _ -> 
        let isFocus =
            match focusDigit with
            | Some d when d = s -> true
            | Some _ -> false
            | None -> true

        if isFocus then
            match currentHintDigitOpt with
            | Some currentHintDigit when currentHintDigit.primaryHintHouse -> 
                ColouredString(s.ToString(), ConsoleColor.Cyan)
            | Some currentHintDigit when currentHintDigit.secondaryHintHouse -> 
                ColouredString(s.ToString(), ConsoleColor.DarkBlue)
            | _ -> ColouredString(s.ToString(), ConsoleColor.Blue)
        else ColouredString(s.ToString(), ConsoleColor.Gray)
    | None, BigNumber _ when not isCentre -> CChar ' '
    | None, BigNumber s -> 
        match currentHintDigitOpt with
        | Some currentHintDigit when currentHintDigit.primaryHintHouse -> 
            ColouredString(s.ToString(), ConsoleColor.Yellow)
        | Some currentHintDigit when currentHintDigit.secondaryHintHouse -> 
            ColouredString(s.ToString(), ConsoleColor.DarkRed)
        | _ -> ColouredString(s.ToString(), ConsoleColor.Red)
    | None, PencilMarks candidates -> 
        let isFocus =
            match focusDigit with
            | Some d when d = candidate -> true
            | Some _ -> false
            | None -> true

        if focusDigit.IsNone then
            match currentHintDigitOpt with
            | Some currentHintDigit when currentHintDigit.setValue.IsSome && currentHintDigit.setValue.Value = candidate -> 
                    ColouredString(candidate.ToString(), ConsoleColor.Red)
            | Some currentHintDigit when currentHintDigit.setValue.IsSome && Set.contains candidate candidates -> 
                ColouredString(candidate.ToString(), ConsoleColor.DarkYellow)
            | Some currentHintDigit when currentHintDigit.setValueReduction.IsSome && currentHintDigit.setValueReduction.Value = candidate && Set.contains candidate candidates -> 
                    ColouredString(candidate.ToString(), ConsoleColor.DarkYellow)
            | Some currentHintDigit when Set.contains candidate currentHintDigit.reductions -> 
                ColouredString(candidate.ToString(), ConsoleColor.DarkYellow)
            | Some currentHintDigit when Set.contains candidate currentHintDigit.pointers -> 
                ColouredString(candidate.ToString(), ConsoleColor.Magenta)
            | Some currentHintDigit when currentHintDigit.primaryHintHouse -> 
                if Set.contains candidate candidates then ColouredString(candidate.ToString(), ConsoleColor.DarkGreen)
                else CChar ' '
            | Some currentHintDigit when currentHintDigit.secondaryHintHouse -> 
                if Set.contains candidate candidates then ColouredString(candidate.ToString(), ConsoleColor.Green)
                else CChar ' '
            | _ -> 
                if Set.contains candidate candidates then CStr(candidate.ToString())
                else CChar ' '
        else
            if focusDigit.Value = candidate then
                if Set.contains candidate candidates then ColouredString(candidate.ToString(), ConsoleColor.Blue)
                else CChar ' '
            else
                if Set.contains candidate candidates then ColouredString(candidate.ToString(), ConsoleColor.Gray)
                else CChar ' '
