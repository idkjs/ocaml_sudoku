module console.format

open core.sudoku

let konst x _ = x

// Printing a row, we need special characters at left, in the middle and on the right
type gridCharsRow<'a> = 
    { l : 'a
      m : 'a
      r : 'a }

// Printing a grid, we need special rows at top, in the middle and on the bottom
// Also, horizontal and vertical spacers
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

// Combine fences with posts (there's one more fence than posts: f p f p ... p f)
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

// Create a sequence of fences interleaved with posts (first and last posts may be different)
// l f p f p f ... p f r
let sinterleave (fenceToSeq : 'a -> seq<'c>) (firstPost : seq<'c>) (midPost : seq<'c>) (lastPost : seq<'c>) 
    (eol : seq<'c>) (fences : 'a list) = 
    seq { 
        yield! firstPost

        yield! simpleInterleave fenceToSeq midPost fences

        yield! lastPost
        yield! eol
    }

// Print a column
let printCell (digitTo : cell -> 'c) cell = digitTo cell |> Seq.singleton

let printColumn (printCell : cell -> seq<'c>) row column : seq<'c> = 
    let cell = 
        { cell.col = column
          row = row }
    printCell cell

// Print a stack
let printStack (columnPrinter : row -> column -> seq<'c>) (columnSeparator : seq<'c>) 
    (puzzleStackColumns : stack -> column list) (row : row) (stack : stack) = 
    simpleInterleave (columnPrinter row) columnSeparator (puzzleStackColumns stack)

// Print a row
let printRow (stackPrinter : stack -> seq<'c>) (gridCharsRow : gridCharsRow<seq<'c>>) eol (stacks : stack list) = 
    seq { 
        yield! gridCharsRow.l
        yield! simpleInterleave stackPrinter gridCharsRow.m stacks
        yield! gridCharsRow.r
        yield! eol
    }

// Print a band
let printBand (rowToSeq : row -> seq<'c>) (rowSeparator : seq<'c>) (puzzleBandRows : band -> row list) (band : band) = 
    simpleInterleave rowToSeq rowSeparator (puzzleBandRows band)

// Print a puzzle grid, supply callback to draw each cell
let printGrid (puzzleStacks : stack list) (puzzleStackColumns : stack -> column list) (puzzleBands : band list) (puzzleBandRows : band -> row list) (gridChars : gridChars<seq<'c>>) (digitTo : cell -> 'c) = 

    let doPrintColumn = printColumn (printCell digitTo)

    let doPrintStack = printStack doPrintColumn Seq.empty puzzleStackColumns

    let doPrintRow row = printRow (doPrintStack row) gridChars.v gridChars.n puzzleStacks

    let doPrintBand = printBand doPrintRow Seq.empty puzzleBandRows

    let r = Seq.collect (konst gridChars.h) (puzzleStackColumns puzzleStacks.Head)
    let printHorizontal (g : gridCharsRow<seq<'c>>) = sinterleave (konst r) g.l g.m g.r gridChars.n puzzleStacks
    let t = printHorizontal gridChars.t
    let m = printHorizontal gridChars.m
    let b = printHorizontal gridChars.b

    sinterleave doPrintBand t m b Seq.empty puzzleBands

let printCandidateGrid (puzzleStacks : stack list) (puzzleStackColumns : stack -> column list) (puzzleBands : band list) (puzzleBandRows : band -> row list) 
    (candidateGridChars : candidateGridChars<seq<'c>>) (alphabet : digit list) 
    (draw_cell : cell -> digit -> 'c) = 

    let d = Seq.collect (konst candidateGridChars.h) (puzzleStackColumns puzzleStacks.Head)
    let i = Seq.collect (konst candidateGridChars.hi) (puzzleStackColumns puzzleStacks.Head)
    
    let printFullHorizontal (x : candidateGridCharsRow<seq<'c>>) i = 
        let s = simpleInterleave (konst i) x.mi (puzzleStackColumns puzzleStacks.Head)

        sinterleave (konst s) x.x.l x.x.m x.x.r candidateGridChars.n puzzleStacks
    
    let c = List.length (puzzleStackColumns puzzleStacks.Head)
    let s = List.toSeq alphabet
    
    let ss = 
        seq { 
            for i in 0..puzzleStacks.Length - 1 do
                yield Seq.skip (i * c) s |> Seq.take c
        }
    
    let sss = List.ofSeq ss
    let ssss = List.map List.ofSeq sss
    
    let doPrintColumn (digits : seq<digit>) = 
        let doPrintCell cell = Seq.map (fun digit -> draw_cell cell digit) digits
        printColumn doPrintCell
    
    let doPrintStack (digits : seq<digit>) = printStack (doPrintColumn digits) candidateGridChars.vi puzzleStackColumns

    let doPrintRow (row : row) = 
        Seq.collect (fun digits -> printRow (doPrintStack digits row) candidateGridChars.v candidateGridChars.n puzzleStacks) 
            ssss
    let t = printFullHorizontal candidateGridChars.t d
    let m = printFullHorizontal candidateGridChars.m d
    let b = printFullHorizontal candidateGridChars.b d

    let rowSeparator = printFullHorizontal candidateGridChars.mi i

    let doPrintBand = printBand doPrintRow rowSeparator puzzleBandRows

    sinterleave doPrintBand t m b Seq.empty puzzleBands
