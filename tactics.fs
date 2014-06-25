module tactics

open sudoku
open puzzlemap



let makeColumn i = { Column.col = i * 1<col> }

let makeRow i = { Row.row = i * 1<row> }

let makeStack i = { Stack.stack = i * 1<boxcol> }

let makeBand i = { Band.band = i * 1<boxrow> }


let cellColumn cell = cell.col

let columnCells length (column:Column) =
    let rr = List.map makeRow [1..length]

    List.map (fun row -> {col = column; row = row}) rr

let cellRow cell = cell.row

let rowCells length (row:Row) =
    let cc = List.map makeColumn [1..length]

    List.map (fun column -> {col = column; row = row}) cc

let columnStack (boxWidth:int<width>) (column:Column) =
    match column with
    | { col = col } -> 1 + ((int) col - 1) / (int)boxWidth |> makeStack

let stackColumns (boxWidth:int<width>) (stack:Stack) =
    let s =
        match stack with
        | { stack = boxcol } -> (int) boxcol

    let ss = [ ((s - 1) * (int)boxWidth + 1) .. s * (int)boxWidth ]
    List.map makeColumn ss

let rowBand (boxHeight:int<height>) (row:Row) =
    match row with
    | { row = row } -> 1 + ((int) row - 1) / (int)boxHeight |> makeBand

let bandRows (boxHeight:int<height>) (band:Band) =
    let b =
        match band with
        | { band = boxrow } -> (int) boxrow

    let bb = [ ((b - 1) * (int)boxHeight + 1) .. b * (int)boxHeight ]
    List.map makeRow bb

let stacks (boxWidth:int<width>) (length:int) =
    List.map makeStack [1..(length / (int)boxWidth)]

let bands (boxHeight:int<height>) (length:int) =
    List.map makeBand [1..(length / (int)boxHeight)]

let boxes (boxWidth:int<width>) (boxHeight:int<height>) (length:int) = 
    let ss = stacks boxWidth length
    let bb = bands boxHeight length

    let all = [
        for band in bb do
            for stack in ss do
                yield { Box.stack = stack; band = band } ]

    all

let columns length =
    List.map makeColumn [1..length]

let rows length =
    List.map makeRow [1..length]

let allCells length = 
    let cc = columns length
    let rr = rows length

    [ for row in rr do
        for column in cc do
            yield { Cell.col = column; row = row } ]


let cons x y = x :: y



type Container<'a, 'b> = ('a * 'b) list


let contentsOfContainer<'a, 'b when 'b : equality> (s : 'b) (sc : Container<'b, 'a>) = List.filter (fun p -> s = fst p) sc |> List.map (fun p -> snd p)

let containerOfContent<'a, 'b when 'a : equality> (sc : Container<'b, 'a>) (c : 'a) = List.find (fun p -> c = snd p) sc |> fst

let makeContainerContainerCell<'a when 'a:equality> (columns:'a list) (allCells:Cell list) (container: Cell -> 'a) = 
    let cc = List.map (fun cell -> (container cell, cell)) allCells
    let ccLookup = (fun b -> contentsOfContainer b cc)
    (cc, ccLookup)

let makeContainerBoxCell (boxes:Box list) (cells:Cell list) boxWidth boxHeight =
    let (boxCells, boxCellLookup) = 
        makeContainerContainerCell<Box> boxes cells
            (fun cell ->
                let col = cell.col
                let stack = columnStack boxWidth col
                let row = cellRow cell
                let band = rowBand boxHeight row
                { Box.band = band; stack = stack })
    let cellBoxLookup = (fun c -> containerOfContent boxCells c)
    (cellBoxLookup, boxCellLookup)

// and v.v.




let entryToAlphabet = function
    | Given(g) -> Some(g)
    | Set(s) -> Some(s)
    | Candidates(_) -> None
