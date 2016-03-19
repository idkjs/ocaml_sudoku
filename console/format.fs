module console.format

open core.smap
open core.sudoku
open core.puzzlemap

let konst x _ = x

(* Printing a row, we need special characters at left, in the middle and on the right *)
type gridCharsRow<'a> = 
    { l : 'a
      m : 'a
      r : 'a }

(* Printing a grid, we need special rows at top, in the middle and on the bottom
 Also, horizontal and vertical spacers *)
type gridChars<'a> = 
    { h : 'a
      v : gridCharsRow<'a>
      t : gridCharsRow<'a>
      m : gridCharsRow<'a>
      b : gridCharsRow<'a>
      n : 'a }

type candidateGridCharsRow<'a> = 
    { mi : 'a
      x : gridCharsRow<'a> }

type candidateGridChars<'a> = 
    { h : 'a
      hi : 'a
      v : gridCharsRow<'a>
      vi : 'a
      t : candidateGridCharsRow<'a>
      m : candidateGridCharsRow<'a>
      mi : candidateGridCharsRow<'a>
      b : candidateGridCharsRow<'a>
      n : 'a }

let printLine (cells : cell list) (digitTo : cell -> 'c) : List<'c> = 
    let cons x y = x :: y
    List.foldBack (digitTo >> cons) cells []

(* Combine fences with posts (there's one more fence than posts: f p f p ... p f) *)
let simpleInterleave (fenceToSeq : 'a -> seq<'c>) (post : seq<'c>) (fences : 'a list) = 
    seq {
        match fences with
        | f :: fs -> 
            yield! fenceToSeq f

            for fence in fs do
                yield! post
                yield! fenceToSeq fence

        | [] -> ()
    }

(* Create a sequence of fences interleaved with posts (first and last posts may be different)
 l f p f p f ... p f r *)
let sinterleave (fenceToSeq : 'a -> seq<'c>) (firstPost : seq<'c>) (midPost : seq<'c>) (lastPost : seq<'c>) 
    (eol : seq<'c>) (fences : 'a list) = 
    seq { 
        yield! firstPost

        yield! simpleInterleave fenceToSeq midPost fences

        yield! lastPost
        yield! eol
    }

(* Print a column *)
let printCell (digitTo : cell -> 'c) cell = digitTo cell |> Seq.singleton

let printColumn (printCell : cell -> seq<'c>) row column : seq<'c> = 
    let cell = makeCell column row
    printCell cell

(* Print a stack *)
let printStack (p : puzzleMap) (columnPrinter : row -> column -> seq<'c>) (columnSeparator : seq<'c>) (row : row) (stack : stack) = 
    simpleInterleave (columnPrinter row) columnSeparator (SMap.get p.stackColumns stack)

(* Print a row *)
let printRow (stackPrinter : stack -> seq<'c>) (gridCharsRow : gridCharsRow<seq<'c>>) eol (stacks : stack list) = 
    seq { 
        yield! gridCharsRow.l
        yield! simpleInterleave stackPrinter gridCharsRow.m stacks
        yield! gridCharsRow.r
        yield! eol
    }

(* Print a band *)
let printBand (p : puzzleMap) (rowToSeq : row -> seq<'c>) (rowSeparator : seq<'c>) (band : band) = 
    simpleInterleave rowToSeq rowSeparator (SMap.get p.bandRows band)

(* Print a puzzle grid, supply callback to draw each cell *)
let printGrid (p : puzzleMap) (gridChars : gridChars<seq<'c>>) (digitTo : cell -> 'c) = 

    let doPrintColumn = printColumn (printCell digitTo)

    let doPrintStack = printStack p doPrintColumn Seq.empty

    let doPrintRow row = printRow (doPrintStack row) gridChars.v gridChars.n p.stacks

    let doPrintBand = printBand p doPrintRow Seq.empty

    let r = Seq.collect (konst gridChars.h) (SMap.get p.stackColumns p.stacks.[0])
    let printHorizontal (g : gridCharsRow<seq<'c>>) = sinterleave (konst r) g.l g.m g.r gridChars.n p.stacks
    let t = printHorizontal gridChars.t
    let m = printHorizontal gridChars.m
    let b = printHorizontal gridChars.b

    sinterleave doPrintBand t m b Seq.empty p.bands

let printCandidateGrid (p : puzzleMap) (candidateGridChars : candidateGridChars<seq<'c>>) (alphabet : digit list) 
    (draw_cell : cell -> digit -> 'c) = 

    let d = Seq.collect (konst candidateGridChars.h) (SMap.get p.stackColumns p.stacks.[0])
    let i = Seq.collect (konst candidateGridChars.hi) (SMap.get p.stackColumns p.stacks.[0])
    
    let printFullHorizontal (x : candidateGridCharsRow<seq<'c>>) i = 
        let s = simpleInterleave (konst i) x.mi (SMap.get p.stackColumns p.stacks.[0])

        sinterleave (konst s) x.x.l x.x.m x.x.r candidateGridChars.n p.stacks
    
    let c = List.length (SMap.get p.stackColumns p.stacks.[0])
    let s = alphabet
    
    let ss = 
        seq { 
            for i in 0..p.stacks.Length - 1 do
                yield Seq.skip (i * c) s |> Seq.take c
        }
    
    let sss = List.ofSeq ss
    let ssss = List.map List.ofSeq sss
    
    let doPrintColumn (digits : seq<digit>) = 
        let doPrintCell cell = Seq.map (fun digit -> draw_cell cell digit) digits
        printColumn doPrintCell
    
    let doPrintStack (digits : seq<digit>) = printStack p (doPrintColumn digits) candidateGridChars.vi

    let doPrintRow (row : row) = 
        Seq.collect (fun digits -> printRow (doPrintStack digits row) candidateGridChars.v candidateGridChars.n p.stacks) 
            ssss
    let t = printFullHorizontal candidateGridChars.t d
    let m = printFullHorizontal candidateGridChars.m d
    let b = printFullHorizontal candidateGridChars.b d

    let rowSeparator = printFullHorizontal candidateGridChars.mi i

    let doPrintBand = printBand p doPrintRow rowSeparator

    sinterleave doPrintBand t m b Seq.empty p.bands
