module format

open sudoku

let konst x _ = x

type gridCharsRow<'a> = {
    l:'a
    m:'a
    r:'a
}

type gridChars<'a> = {
    h:'a
    v:'a
    t:gridCharsRow<'a>
    m:gridCharsRow<'a>
    b:gridCharsRow<'a>
    nl:'a
}

type solutionCharsRow<'a> = {
    mi:'a
    x:gridCharsRow<'a>
}

type solutionChars<'a> = {
    h:'a
    hi:'a
    v:'a
    vi:'a
    t:solutionCharsRow<'a>
    m:solutionCharsRow<'a>
    mi:solutionCharsRow<'a>
    b:solutionCharsRow<'a>
    nl:'a
}

// Print out sudoku as a long string of gridSize*gridSize Symbols

let seqInterleave (fenceToSeq:'a->seq<'c>) (midPost:seq<'c>) (fences:'a list) =
    seq {
        match fences with
        | f :: fs ->
            yield! fenceToSeq f

            for fence in fs do
                yield! midPost
                yield! fenceToSeq fence

        | [] -> ()
    }

// Create a sequence of fences interleaved with posts (first and last posts may be different)
let sinterleave (fenceToSeq:'a->seq<'c>) (firstPost:seq<'c>) (midPost:seq<'c>) (lastPost:seq<'c>) (eol:seq<'c>) (fences:'a list) =
    seq {
        yield! firstPost

        match fences with
        | f :: fs ->
            yield! fenceToSeq f

            for fence in fs do
                yield! midPost
                yield! fenceToSeq fence

        | [] -> ()

        yield! lastPost
        yield! eol
    }

let printRowOnOneLine (symbolTo:'b->'c) (containerItems:'a->'b list) (midPost:seq<'c>) (containers:'a list) =
    let containerToSeq container = Seq.map symbolTo (containerItems container)

    sinterleave containerToSeq midPost midPost midPost Seq.empty containers

// Print a column
let printCell (symbolTo:Cell->'c) cell =
    symbolTo cell |> Seq.singleton

let printColumn (printCell:Cell->seq<'c>) row column =
    let cell = { Cell.col = column; row = row }
    printCell cell

// Print a stack
let printStack (columnPrinter:Row->Column->seq<'c>) (columnSeparator:seq<'c>) (stackColumns:Stack->Column list) row (stack:Stack) =
    seqInterleave (columnPrinter row) columnSeparator (stackColumns stack)

// Print a row
let printRow (stackPrinter:Stack->seq<'c>) (stackSeparator:seq<'c>) nl (stacks:Stack list) =
    sinterleave stackPrinter stackSeparator stackSeparator stackSeparator nl stacks

// Print a band
let printBand (rowToSeq:Row->seq<'c>) (rowSeparator:seq<'c>) (bandRows:Band->Row list) (band:Band) =
    seqInterleave rowToSeq rowSeparator (bandRows band)

// Print a puzzle grid, supply callback to draw each cell
let print_long (gridChars:gridChars<seq<'c>>) (symbolTo:Cell->'c) (stacks:Stack list) (stackColumns:Stack->Column list) (bands:Band list) (bandRows:Band->Row list) =

    let doPrintColumn  = printColumn (printCell symbolTo)

    let doPrintStack = printStack doPrintColumn Seq.empty stackColumns

    let doPrintRow row = printRow (doPrintStack row) gridChars.v gridChars.nl stacks

    let doPrintBand = printBand doPrintRow Seq.empty bandRows

    let r = Seq.collect (konst gridChars.h) (stackColumns stacks.Head)

    let printHorizontal (g:gridCharsRow<seq<'c>>) =
        sinterleave (konst r) g.l g.m g.r gridChars.nl stacks

    let t = printHorizontal gridChars.t
    let m = printHorizontal gridChars.m
    let b = printHorizontal gridChars.b

    sinterleave doPrintBand t m b Seq.empty bands

let print_full (solutionChars:solutionChars<seq<'c>>) (symbolTo:Cell->Symbol->'c) (stacks:Stack list) stackColumns (bands:Band list) (bandRows:Band -> Row list) (alphabet : Alphabet) =

    let d = Seq.collect (konst solutionChars.h) (stackColumns stacks.Head)
    let i = Seq.collect (konst solutionChars.hi) (stackColumns stacks.Head)

    let printFullHorizontal (x:solutionCharsRow<seq<'c>>) i =
        let s = seqInterleave (konst i) x.mi (stackColumns stacks.Head)

        sinterleave (konst s) x.x.l x.x.m x.x.r solutionChars.nl stacks


    let c = List.length (stackColumns stacks.Head)
    let s = List.toSeq alphabet
    let ss = seq { for i in 0 .. stacks.Length - 1 do yield Seq.skip (i * c) s |> Seq.take c }
    let sss = List.ofSeq ss
    let ssss = List.map List.ofSeq sss

    let doPrintColumn symbols =
        let doPrintCell cell = Seq.map (symbolTo cell) symbols
        printColumn doPrintCell

    let doPrintStack symbols = printStack (doPrintColumn symbols) solutionChars.vi stackColumns

    let doPrintRow row =
        Seq.collect (
            fun symbols ->
                printRow (doPrintStack symbols row) solutionChars.v solutionChars.nl stacks)
            ssss

    let t = printFullHorizontal solutionChars.t d
    let m = printFullHorizontal solutionChars.m d
    let b = printFullHorizontal solutionChars.b d

    let rowSeparator = printFullHorizontal solutionChars.mi i

    let doPrintBand = printBand doPrintRow rowSeparator bandRows


    sinterleave doPrintBand t m b Seq.empty bands

