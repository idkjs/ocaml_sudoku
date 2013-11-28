#light

module console

open System
open System.Text

open sudoku
open tactics

// Things we may want to write
type ConsoleChar =
    | CChar of char
    | CStr of string
    | ColouredChar of char * ConsoleColor
    | NL

// and how to write them
type ConsoleCharWriter = ConsoleChar -> Unit

// Some predefined characters - for smaller grid
let gridLeftRightChar = (CChar >> cons) '│'

let gridTopChars = Array.map (CChar >> cons) [| '┌'; '─'; '┬'; '┐' |]

let gridMidChars = Array.map (CChar >> cons) [| '├'; '─'; '┼'; '┤' |]

let gridBottomChars = Array.map (CChar >> cons) [| '└'; '─'; '┴'; '┘' |]

// ... and for larger grid
let solutionStepLeftRightChar = (CChar >> cons) '║'

let solutionStepLeftRightBoxChar = (CChar >> cons) '│'

let solutionStepTopChars = Array.map (CChar >> cons) [|'╔'; '═'; '╦'; '╦'; '╗' |]

let solutionStepMidChars = Array.map (CChar >> cons) [| '╠'; '─'; '┼'; '╬'; '╣' |]

let solutionStepBoxChars = Array.map (CChar >> cons) [| '╠'; '═'; '╬'; '╬'; '╣' |]

let solutionStepBottomChars = Array.map (CChar >> cons) [| '╚'; '═'; '╧'; '╩'; '╝' |]

// Change the console colour to write a string
let ConsoleWriteColor (value:char) consoleColour =
    let foregroundColour = System.Console.ForegroundColor
    try
        System.Console.ForegroundColor <- consoleColour
        System.Console.Write value
    finally
        System.Console.ForegroundColor <- foregroundColour

let ConsoleWriteChar = function
    | CChar c -> Console.Write c
    | CStr c -> Console.Write c
    | ColouredChar (c, consoleColour) -> ConsoleWriteColor c consoleColour
    | NL -> Console.WriteLine ""

let PlainWriteCharBuilder (sb:StringBuilder) =
    fun (cc:ConsoleChar) ->
        match cc with
        | CChar c -> sb.Append c |> ignore
        | CStr s -> sb.Append s |> ignore
        | ColouredChar (c, _) -> sb.Append c |> ignore
        | NL -> sb.AppendLine "" |> ignore

// Print a symbol option, with colours
let symbolOptionToConsoleChar = function
    | Some(Symbol s) -> ColouredChar (s, ConsoleColor.Blue)
    | None -> CChar '.'

// Print an entry, with colours
let entryToConsole = function
    | Given(Symbol s) -> ColouredChar (s, ConsoleColor.Blue)
    | Set(Symbol s) -> ColouredChar (s, ConsoleColor.Red)
    | Candidates(_) -> CChar '.'

// Print an entry for a candidate to console
let entryAndCandidateToConsole candidate isCentre = function
    | Given(Symbol s) ->
        if isCentre then
            ColouredChar (s, ConsoleColor.Blue)
        else
            CChar ' '
    | Set(Symbol s) ->
        if isCentre then
            ColouredChar (s, ConsoleColor.Red)
        else
            CChar ' '
    | Candidates(candidates) ->
        if Set.contains candidate candidates then
            let (Symbol s) = candidate
            CChar s
        else
            CChar ' '

// Print out sudoku as a long string of gridSize*gridSize Symbols
let interleave2 (folder:'a -> 'c list -> 'c list) (chMid:'c list -> 'c list) (fences:'a list) (acc:'c list) =

    let innerFolder fence acc2 = folder fence acc2 |> chMid

    List.foldBack innerFolder (List.tail fences) acc

    |> folder (List.head fences)

let interleave (folder:'a -> 'c list -> 'c list) (chBegin:'c list -> 'c list) (chMid:'c list -> 'c list) (chEnd:'c list -> 'c list) (fences:'a list) (acc:'c list) =
    acc
    |> chEnd
    |> interleave2 folder chMid fences
    |> chBegin

let printRowOnOneLine symbolTo (containerItems:'b -> 'c list) eol =

    let folder = List.foldBack symbolTo << containerItems

    interleave folder eol eol eol

let print_horizontal_divider (chars:('a list -> 'a list) array) eol (containerItems:'b -> 'c list) =
    let chDiv _ = chars.[1]
    let chBegin = chars.[0]
    let chMid = chars.[2]
    let chEnd acc = chars.[3] (NL :: acc)

    let folder = List.foldBack chDiv << containerItems

    interleave folder chBegin chMid chEnd


let printFullHorizontal (chars:'a array) (containers:'b list) (containerItems:'b -> 'c list) acc1 =
    let chBegin = chars.[0] 
    let chDiv _ = chars.[1]
    let chMid = chars.[2]
    let chMid2 = chars.[3]
    let chEnd acc = chars.[4] (NL :: acc)

    let outerFolder container acc =
        let items = containerItems container
        let folder _ acc = List.foldBack chDiv items acc

        interleave2 folder chMid items acc

    interleave outerFolder chBegin chMid2 chEnd containers acc1


// Print a puzzle grid, supply callback to draw each cell
let print_long (symbolTo:Cell -> 'c list -> 'c list) (eol:'c list -> 'c list) (stacks:Stack list) stackColumns (bands:Band list) (bandRows:Band->Row list) =

    let stackToCells columnToCells = stackColumns >> List.map columnToCells

    let folder columnToCells = stackToCells columnToCells >> List.foldBack symbolTo

    let rowFolder row accRow =
        let folderr = makeCell2 row |> folder

        interleave folderr gridLeftRightChar gridLeftRightChar gridLeftRightChar stacks (NL :: accRow)

    let bandFolder = bandRows >> List.foldBack rowFolder

    let a = print_horizontal_divider gridTopChars eol stackColumns stacks
    let b = print_horizontal_divider gridMidChars eol stackColumns stacks
    let c = print_horizontal_divider gridBottomChars eol stackColumns stacks

    interleave bandFolder a b c bands

let printColumnRow (columnToSymbolTo:Column -> Symbol -> bool -> 'a list -> 'a list) row (columnLength:int) (rowLength:int) subrow alphabet column =

    List.foldBack
        (fun index acc ->
            let candidate = List.nth alphabet (subrow * columnLength + index)
            let centre = subrow = rowLength / 2 && index = columnLength / 2

            columnToSymbolTo column candidate centre acc
        )
        [0 .. columnLength - 1]

let printRow (symbolTo:Cell -> 'a) row (stacks:Stack list) (stackColumns : Stack -> Column list) (rowLength:int) alphabet subrow acc =

    let outerFolder stack acc2 =
        let columnToSymbolTo column = makeCell2 row column |> symbolTo
        let c column candidate centre = cons (entryAndCandidateToConsole candidate centre (columnToSymbolTo column ))

        let columns = stackColumns stack
        let printer = printColumnRow c row columns.Length rowLength subrow alphabet

        interleave2 printer solutionStepLeftRightBoxChar columns acc2

    interleave outerFolder solutionStepLeftRightChar solutionStepLeftRightChar solutionStepLeftRightChar stacks (NL :: acc)

let printBand (symbolTo:Cell -> 'a)  (stacks:Stack list) (stackColumns : Stack -> Column list) (bands:Band list) (bandRows:Band -> Row list) alphabet band =

    let rows = bandRows band

    let folder row =
        List.foldBack
            (fun index acc3 ->
                printRow symbolTo row stacks stackColumns rows.Length alphabet index acc3
            )
            [ 0 .. rows.Length - 1 ]

    let h2 = printFullHorizontal solutionStepMidChars stacks stackColumns

    interleave2 folder h2 rows


let print_full symbolTo stacks stackColumns (bands:Band list) (bandRows:Band -> Row list) (alphabet : Alphabet) =

    let h1 = printFullHorizontal solutionStepTopChars stacks stackColumns
    let h3 = printFullHorizontal solutionStepBoxChars stacks stackColumns
    let h4 = printFullHorizontal solutionStepBottomChars stacks stackColumns

    let printBand = printBand symbolTo stacks stackColumns bands bandRows alphabet

    interleave printBand h1 h3 h4 bands

let loadLine (line:string) alphabet = List.map (charToAlphabet alphabet) (List.ofSeq line)

// Load a sudoku given as a single line of gridSize*gridSize characters
let loadPuzzle (alphabetisedLine:Symbol option list) (cells:Cell list) =

    let grid = new System.Collections.Generic.Dictionary<Cell, Symbol option>()

    List.iter2 (fun cell c -> grid.Add(cell, c) ) cells alphabetisedLine

    let puzzleGridCellLookup = fun cell -> grid.[cell]

    puzzleGridCellLookup
