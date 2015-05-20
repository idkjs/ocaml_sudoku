module core.test

open core.sudoku
open core.puzzlemap

open NUnit.Framework

[<Test>]
let ``Can make columns``() = 
    let actual : Column list = columns (9 * 1<size>)

    let expected : Column list =
        [1..9]
        |> List.map
            (fun c ->
                { Column.col = c * 1<column> })

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Can make rows``() = 
    let actual : Row list = rows (9 * 1<size>)

    let expected : Row list =
        [1..9]
        |> List.map
            (fun r ->
                { Row.row = r * 1<row> })

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Can make cells``() = 
    let actual : Cell list = cells (9 * 1<size>)

    let expected : Cell list =
        [1..9]
        |> List.collect
            (fun r ->
                [1..9]
                |> List.map
                    (fun c ->
                        { Cell.col = { Column.col = c * 1<column>}
                          row = { Row.row = r * 1<row>} }))

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Can make stacks``() = 
    let actual : Stack list = stacks (8 * 1<size>) (2 * 1<boxWidth>)

    let expected : Stack list =
        [1..4]
        |> List.map
            (fun s ->
                { Stack.stack = s * 1<stack> })

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Can make bands``() = 
    let actual : Band list = bands (8 * 1<size>) (4 * 1<boxHeight>)

    let expected : Band list =
        [1..2]
        |> List.map
            (fun b ->
                { Band.band = b * 1<band> })

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Can make boxes``() = 
    let actual : Box list = boxes (9 * 1<size>) (2 * 1<boxWidth>) (4 * 1<boxHeight>)

    let expected : Box list =
        [1..2]
        |> List.collect
            (fun b ->
                [1..4]
                |> List.map
                    (fun s ->
                        { Box.stack = { Stack.stack = s * 1<stack>}
                          band = { Band.band = b * 1<band>} }))

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Can make houses``() = 
    let actual : House list = houses (9 * 1<size>) (2 * 1<boxWidth>) (4 * 1<boxHeight>)

    let expectedColumns : House list =
        [1..9]
        |> List.map
            (fun c ->
                { Column.col = c * 1<column> })
        |> List.map HColumn

    let expectedRows : House list =
        [1..9]
        |> List.map
            (fun r ->
                { Row.row = r * 1<row> })
        |> List.map HRow

    let expectedBoxes : House list =
        [1..2]
        |> List.collect
            (fun b ->
                [1..4]
                |> List.map
                    (fun s ->
                        { Box.stack = { Stack.stack = s * 1<stack>}
                          band = { Band.band = b * 1<band>} }))
        |> List.map HBox

    let expected =
        [ expectedColumns; expectedRows; expectedBoxes]
        |> List.concat

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Get column cells``() = 
    let actual : Cell list = columnCells (9 * 1<size>) { Column.col = 2 * 1<column> }

    let expected : Cell list =
        [1..9]
        |> List.map
            (fun r ->
                { Cell.col = { Column.col = 2 * 1<column> }
                  row = { Row.row = r * 1<row> }})

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Get row cells``() = 
    let actual : Cell list = rowCells (9 * 1<size>) { Row.row = 7 * 1<row> }

    let expected : Cell list =
        [1..9]
        |> List.map
            (fun c ->
                { Cell.col = { Column.col = c * 1<column> }
                  row = { Row.row = 7 * 1<row> }})

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Get stack for a column``() =
    let columns : Column list = columns (9 * 1<size>)
    let actual : Stack list =
        columns
        |> List.map (columnStack (3 * 1<boxWidth>))

    let expected : Stack list =
        [1..3]
        |> List.collect
            (fun s ->
                [1..3]
                |> List.map
                    (fun _ ->
                        { Stack.stack = s * 1<stack> }))

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Get stack columns``() = 
    let actual : Column list = stackColumns (3 * 1<boxWidth>) { Stack.stack = 2 * 1<stack> }

    let expected : Column list =
        [4..6]
        |> List.map
            (fun c ->
                { Column.col = c * 1<column> })

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Get band for a row``() =
    let rows : Row list = rows (9 * 1<size>)
    let actual : Band list =
        rows
        |> List.map (rowBand (3 * 1<boxHeight>))

    let expected : Band list =
        [1..3]
        |> List.collect
            (fun b ->
                [1..3]
                |> List.map
                    (fun _ ->
                        { Band.band = b * 1<band> }))

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Get band rows``() = 
    let actual : Row list = bandRows (3 * 1<boxHeight>) { Band.band = 2 * 1<band> }

    let expected : Row list =
        [4..6]
        |> List.map
            (fun r ->
                { Row.row = r * 1<row> })

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Get box for a cell``() =
    let cells : Cell list =
        [1..9]
        |> List.map
            (fun r ->
                { Cell.col = { Column.col = 5 * 1<column>}
                  row = { Row.row = r * 1<row>} })

    let actual : Box list =
        cells
        |> List.map (cellBox (3 * 1<boxWidth>) (3 * 1<boxHeight>))

    let expected : Box list =
        [1..3]
        |> List.collect
            (fun b ->
                [1..3]
                |> List.map
                    (fun _ ->
                        { Box.stack = { Stack.stack = 2 * 1<stack>}
                          band = { Band.band = b * 1<band>} }))

    Assert.AreEqual(expected, actual)

