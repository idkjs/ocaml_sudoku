open Sudoku
open Puzzlemap
(*F# open FSharp.Compatibility.OCaml F#*)

type basic_color =
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
    | DarkRed
    | DarkGreen
    | DarkYellow
    | DarkBlue

(* Things we may want to write *)
type consoleChar = 
    | CNil
    | CChar of char
    | CStr of string
    | ColouredChar of char * basic_color
    | ColouredString of string * basic_color
    | NL

type consoleString = consoleChar list

(* Printing a row, we need special characters at left, in the middle and on the right *)
type gridCharsRow = 
    { l : consoleString;
      m : consoleString;
      r : consoleString }

(* Printing a grid, we need special rows at top, in the middle and on the bottom
 Also, horizontal and vertical spacers *)
type gridChars = 
    { h : consoleString;
      v : gridCharsRow;
      t : gridCharsRow;
      m : gridCharsRow;
      b : gridCharsRow;
      n : consoleString }

type candidateGridCharsRow = 
    { mi : consoleString;
      x : gridCharsRow }

type candidateGridChars = 
    { h : consoleString;
      hi : consoleString;
      v : gridCharsRow;
      vi : consoleString;
      t : candidateGridCharsRow;
      m : candidateGridCharsRow;
      mi : candidateGridCharsRow;
      b : candidateGridCharsRow;
      n : consoleString }

let konst x _ = x

let printLine (cells : cells) (digitTo : cell -> consoleString) : consoleString = 
    cells
    |> Cells.toList
    |> List.collect digitTo

(* Combine fences with posts (there's one more fence than posts: f p f p ... p f) *)
let simpleInterleave (fenceToSeq : 'a -> consoleString) (post : consoleString) (fences : 'a list) : consoleString = 
    let rec gen (fences' : 'a list) : consoleString = 
        match fences' with
        | [] -> []
        | [f] -> fenceToSeq f
        | f :: fs -> List.concat [(fenceToSeq f); post; (gen fs)]
        in
     gen fences

(* Create a sequence of fences interleaved with posts (first and last posts may be different)
 l f p f p f ... p f r *)
let sinterleave (fenceToSeq : 'a -> consoleString) (firstPost : consoleString) (midPost : consoleString) (lastPost : consoleString) (eol : consoleString) (fences : 'a list) : consoleString = 
    List.concat [firstPost; simpleInterleave fenceToSeq midPost fences; lastPost; eol]

(* Print a column *)
let printCell (digitTo : cell -> consoleString) (cell : cell) : consoleString = 
    digitTo cell

let printColumn (printCell : cell -> consoleString) (row : row) (column : column) : consoleString = 
    let cell = Cell.make column row in
    printCell cell

(* Print a stack *)
let printStack (p : puzzleMap) (columnPrinter : row -> column -> consoleString) (columnSeparator : consoleString) (row : row) (stack : stack) : consoleString = 
    simpleInterleave (columnPrinter row) columnSeparator (Smap.get Stack.comparer p.stackColumns stack)

(* Print a row *)
let printRow (stackPrinter : stack -> consoleString) (gridCharsRow : gridCharsRow) (eol : consoleString) (stacks : stack list) : consoleString = 
    List.concat [gridCharsRow.l; simpleInterleave stackPrinter gridCharsRow.m stacks; gridCharsRow.r; eol ]

(* Print a band *)
let printBand (p : puzzleMap) (rowToSeq : row -> consoleString) (rowSeparator : consoleString) (band : band) : consoleString = 
    simpleInterleave rowToSeq rowSeparator (Smap.get Band.comparer p.bandRows band)

(* Print a puzzle grid, supply callback to draw each cell *)
let printGrid (p : puzzleMap) (gridChars : gridChars) (digitTo : cell -> consoleString) : consoleString = 

    let doPrintColumn : row -> column -> consoleString = printColumn (printCell digitTo) in

    let doPrintStack : row -> stack -> consoleString = printStack p doPrintColumn [] in

    let doPrintRow : row -> consoleString = fun row -> printRow (doPrintStack row) gridChars.v gridChars.n p.stacks in

    let doPrintBand : band -> consoleString = printBand p doPrintRow [] in

    let r : consoleString = List.collect (konst gridChars.h) (Smap.get Stack.comparer p.stackColumns p.stacks.[0]) in

    let printHorizontal (g : gridCharsRow) : consoleString = sinterleave (konst r) g.l g.m g.r gridChars.n p.stacks in

    let t = printHorizontal gridChars.t in
    let m = printHorizontal gridChars.m in
    let b = printHorizontal gridChars.b in

    sinterleave doPrintBand t m b [] p.bands

let printCandidateGrid (p : puzzleMap) (candidateGridChars : candidateGridChars) (alphabet : digits) (draw_cell : cell -> digit -> consoleString) : consoleString = 

    let d : consoleString = List.collect (konst candidateGridChars.h) (Smap.get Stack.comparer p.stackColumns p.stacks.[0]) in
    let i : consoleString = List.collect (konst candidateGridChars.hi) (Smap.get Stack.comparer p.stackColumns p.stacks.[0]) in

    let printFullHorizontal (x : candidateGridCharsRow) (i : consoleString) : consoleString = 
        let s = simpleInterleave (konst i) x.mi (Smap.get Stack.comparer p.stackColumns p.stacks.[0]) in

        sinterleave (konst s) x.x.l x.x.m x.x.r candidateGridChars.n p.stacks
        in

    let c : int = List.length (Smap.get Stack.comparer p.stackColumns p.stacks.[0]) in
    let s : digit list = Digits.toList alphabet in
    
    let ss : digit list list = 
        Sset.range 0 (p.stacks.Length - 1)
        |> List.map (fun i -> Seq.skip (i * c) s |> Seq.take c |> Seq.toList)
        in

    let doPrintColumn (digits : digit list) : row -> column -> consoleString = 
        let doPrintCell : cell -> consoleString = fun cell -> List.collect (fun digit -> draw_cell cell digit) digits in
        printColumn doPrintCell
        in

    let doPrintStack (digits : digit list) : row -> stack -> consoleString =
        printStack p (doPrintColumn digits) candidateGridChars.vi
        in

    let doPrintRow (row : row) : consoleString = 
        ss
        |> List.collect
            (fun digits -> printRow (doPrintStack digits row) candidateGridChars.v candidateGridChars.n p.stacks) 
        in

    let t : consoleString = printFullHorizontal candidateGridChars.t d in
    let m : consoleString = printFullHorizontal candidateGridChars.m d in
    let b : consoleString = printFullHorizontal candidateGridChars.b d in

    let rowSeparator : consoleString = printFullHorizontal candidateGridChars.mi i in

    let doPrintBand : band -> consoleString = printBand p doPrintRow rowSeparator in

    sinterleave doPrintBand t m b [] p.bands
