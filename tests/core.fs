module core.test

open core.sudoku
open core.puzzlemap

open NUnit.Framework

[<Test>]
let ``Can make columns``() = 
    let actual : Set<Column> = columns (9 * 1<size>)

    let expected : Set<Column> =
        [1..9]
        |> Set.ofList
        |> Set.map
            (fun c ->
                c * 1<column> |> CColumn)

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Can make rows``() = 
    let actual : Set<Row> = rows (9 * 1<size>)

    let expected : Set<Row> =
        [1..9]
        |> Set.ofList
        |> Set.map
            (fun r ->
                r * 1<row> |> RRow)

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Can make cells``() = 
    let actual : Set<Cell> = cells (9 * 1<size>)

    let expected : Set<Cell> =
        [1..9]
        |> Set.ofList
        |> Set.map
            (fun r ->
                [1..9]
                |> Set.ofList
                |> Set.map
                    (fun c ->
                        { Cell.col = (c * 1<column> |> CColumn)
                          row = (r * 1<row> |> RRow) }))
        |> Set.unionMany

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Can make stacks``() = 
    let actual : Set<Stack> = stacks (8 * 1<size>) (2 * 1<boxWidth>)

    let expected : Set<Stack> =
        [1..4]
        |> Set.ofList
        |> Set.map
            (fun s ->
                s * 1<stack> |> SStack)

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Can make bands``() = 
    let actual : Set<Band> = bands (8 * 1<size>) (4 * 1<boxHeight>)

    let expected : Set<Band> =
        [1..2]
        |> Set.ofList
        |> Set.map
            (fun b ->
                b * 1<band> |> BBand )

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Can make boxes``() = 
    let actual : Set<Box> = boxes (9 * 1<size>) (2 * 1<boxWidth>) (4 * 1<boxHeight>)

    let expected : Set<Box> =
        [1..2]
        |> Set.ofList
        |> Set.map
            (fun b ->
                [1..4]
                |> Set.ofList
                |> Set.map
                    (fun s ->
                        { Box.stack = s * 1<stack> |> SStack
                          band = b * 1<band> |> BBand }))
        |> Set.unionMany

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Can make houses``() = 
    let actual : Set<House> = houses (9 * 1<size>) (2 * 1<boxWidth>) (4 * 1<boxHeight>)

    let expectedColumns : Set<House> =
        [1..9]
        |> Set.ofList
        |> Set.map
            (fun c ->
                c * 1<column> |> CColumn)
        |> Set.map HColumn

    let expectedRows : Set<House> =
        [1..9]
        |> Set.ofList
        |> Set.map
            (fun r ->
                r * 1<row> |> RRow)
        |> Set.map HRow

    let expectedBoxes : Set<House> =
        [1..2]
        |> Set.ofList
        |> Set.map
            (fun b ->
                [1..4]
                |> Set.ofList
                |> Set.map
                    (fun s ->
                        { Box.stack = s * 1<stack> |> SStack
                          band = b * 1<band> |> BBand }))
        |> Set.unionMany
        |> Set.map HBox

    let expected =
        [ expectedColumns; expectedRows; expectedBoxes]
        |> Set.ofList
        |> Set.unionMany

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Get column cells``() = 
    let actual : Set<Cell> = columnCells (9 * 1<size>) (2 * 1<column> |> CColumn)

    let expected : Set<Cell> =
        [1..9]
        |> Set.ofList
        |> Set.map
            (fun r ->
                { Cell.col = (2 * 1<column> |> CColumn)
                  row = (r * 1<row> |> RRow) })

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Get row cells``() = 
    let actual : Set<Cell> = rowCells (9 * 1<size>) (7 * 1<row> |> RRow)

    let expected : Set<Cell> =
        [1..9]
        |> Set.ofList
        |> Set.map
            (fun c ->
                { Cell.col = (c * 1<column> |> CColumn)
                  row = (7 * 1<row> |> RRow) })

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Get stack for a column``() =
    let columns : Set<Column> = columns (9 * 1<size>)
    let actual : Set<Stack> =
        columns
        |> Set.map (columnStack (3 * 1<boxWidth>))

    let expected : Set<Stack> =
        [1..3]
        |> Set.ofList
        |> Set.map
            (fun s ->
                [1..3]
                |> Set.ofList
                |> Set.map
                    (fun _ ->
                        s * 1<stack> |> SStack))
        |> Set.unionMany

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Get stack columns``() = 
    let actual : Set<Column> = stackColumns (3 * 1<boxWidth>) (2 * 1<stack> |> SStack)

    let expected : Set<Column> =
        [4..6]
        |> Set.ofList
        |> Set.map
            (fun c ->
                c * 1<column> |> CColumn)

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Get band for a row``() =
    let rows : Set<Row> = rows (9 * 1<size>)
    let actual : Set<Band> =
        rows
        |> Set.map (rowBand (3 * 1<boxHeight>))

    let expected : Set<Band> =
        [1..3]
        |> Set.ofList
        |> Set.map
            (fun b ->
                [1..3]
                |> Set.ofList
                |> Set.map
                    (fun _ ->
                        b * 1<band> |> BBand))
        |> Set.unionMany

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Get band rows``() = 
    let actual : Set<Row> = bandRows (3 * 1<boxHeight>) (2 * 1<band> |> BBand)

    let expected : Set<Row> =
        [4..6]
        |> Set.ofList
        |> Set.map
            (fun r ->
                r * 1<row> |> RRow)

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Get box for a cell``() =
    let cells : Set<Cell> =
        [1..9]
        |> Set.ofList
        |> Set.map
            (fun r ->
                { Cell.col = (5 * 1<column> |> CColumn)
                  row = (r * 1<row> |> RRow) })

    let actual : Set<Box> =
        cells
        |> Set.map (cellBox (3 * 1<boxWidth>) (3 * 1<boxHeight>))

    let expected : Set<Box> =
        [1..3]
        |> Set.ofList
        |> Set.map
            (fun b ->
                [1..3]
                |> Set.ofList
                |> Set.map
                    (fun _ ->
                        { Box.stack = 2 * 1<stack> |> SStack
                          band = (b * 1<band>) |> BBand }))
        |> Set.unionMany

    Assert.AreEqual(expected, actual)

