module core.test

open core.sudoku
open core.puzzlemap

open NUnit.Framework

let defaultPuzzleSpec = 
    { size = 9
      boxWidth = 3
      boxHeight = 3
      alphabet = 
          [ for i in 1..9 -> (char) i + '0'
                             |> Digit ] }

let twoByFourPuzzleSpec =
    { size = 8
      boxWidth = 2
      boxHeight = 4
      alphabet = 
          [ for i in 1..8 -> (char) i + '0'
                             |> Digit ] }

[<Test>]
let ``Can make columns``() =
    let p = TPuzzleMap defaultPuzzleSpec :> PuzzleMap

    let actual : Set<Column> = p.columns

    let expected : Set<Column> =
        [1..9]
        |> Set.ofList
        |> Set.map
            (fun c ->
                c |> CColumn)

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Can make rows``() = 
    let p = TPuzzleMap defaultPuzzleSpec :> PuzzleMap

    let actual : Set<Row> = p.rows

    let expected : Set<Row> =
        [1..9]
        |> Set.ofList
        |> Set.map
            (fun r ->
                r |> RRow)

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Can make cells``() = 
    let p = TPuzzleMap defaultPuzzleSpec :> PuzzleMap

    let actual : Set<Cell> = p.cells

    let expected : Set<Cell> =
        [1..9]
        |> Set.ofList
        |> Set.map
            (fun r ->
                [1..9]
                |> Set.ofList
                |> Set.map
                    (fun c ->
                        { Cell.col = c |> CColumn
                          row = r |> RRow }))
        |> Set.unionMany

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Can make stacks``() = 
    let p = TPuzzleMap twoByFourPuzzleSpec :> PuzzleMap

    let actual : Set<Stack> = p.stacks

    let expected : Set<Stack> =
        [1..4]
        |> Set.ofList
        |> Set.map
            (fun s ->
                s |> SStack)

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Can make bands``() = 
    let p = TPuzzleMap twoByFourPuzzleSpec :> PuzzleMap

    let actual : Set<Band> = p.bands

    let expected : Set<Band> =
        [1..2]
        |> Set.ofList
        |> Set.map
            (fun b ->
                b |> BBand )

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Can make boxes``() = 
    let p = TPuzzleMap twoByFourPuzzleSpec :> PuzzleMap

    let actual : Set<Box> = p.boxes

    let expected : Set<Box> =
        [1..2]
        |> Set.ofList
        |> Set.map
            (fun b ->
                [1..4]
                |> Set.ofList
                |> Set.map
                    (fun s ->
                        { Box.stack = s |> SStack
                          band = b |> BBand }))
        |> Set.unionMany

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Can make houses``() = 
    let p = TPuzzleMap twoByFourPuzzleSpec :> PuzzleMap

    let actual : Set<House> = p.houses

    let expectedColumns : Set<House> =
        [1..9]
        |> Set.ofList
        |> Set.map
            (fun c ->
                c |> CColumn)
        |> Set.map HColumn

    let expectedRows : Set<House> =
        [1..9]
        |> Set.ofList
        |> Set.map
            (fun r ->
                r |> RRow)
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
                        { Box.stack = s |> SStack
                          band = b |> BBand }))
        |> Set.unionMany
        |> Set.map HBox

    let expected =
        [ expectedColumns; expectedRows; expectedBoxes]
        |> Set.ofList
        |> Set.unionMany

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Get column cells``() = 
    let p = TPuzzleMap defaultPuzzleSpec :> PuzzleMap

    let actual : Set<Cell> = p.columnCells.Get (2 |> CColumn)

    let expected : Set<Cell> =
        [1..9]
        |> Set.ofList
        |> Set.map
            (fun r ->
                { Cell.col = 2 |> CColumn
                  row = r |> RRow })

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Get row cells``() = 
    let p = TPuzzleMap defaultPuzzleSpec :> PuzzleMap

    let actual : Set<Cell> = p.rowCells.Get (7 |> RRow)

    let expected : Set<Cell> =
        [1..9]
        |> Set.ofList
        |> Set.map
            (fun c ->
                { Cell.col = c |> CColumn
                  row = 7 |> RRow })

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Get stack for a column``() =
    let p = TPuzzleMap defaultPuzzleSpec :> PuzzleMap

    let columns : Set<Column> = p.columns

    let actual : Set<Stack> =
        columns
        |> Set.map p.columnStack.Get

    let expected : Set<Stack> =
        [1..3]
        |> Set.ofList
        |> Set.map
            (fun s ->
                [1..3]
                |> Set.ofList
                |> Set.map
                    (fun _ ->
                        s |> SStack))
        |> Set.unionMany

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Get stack columns``() = 
    let p = TPuzzleMap defaultPuzzleSpec :> PuzzleMap

    let actual : Set<Column> = p.stackColumns.Get (2 |> SStack)

    let expected : Set<Column> =
        [4..6]
        |> Set.ofList
        |> Set.map
            (fun c ->
                c |> CColumn)

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Get band for a row``() =
    let p = TPuzzleMap defaultPuzzleSpec :> PuzzleMap

    let rows : Set<Row> = p.rows

    let actual : Set<Band> =
        rows
        |> Set.map p.rowBand.Get

    let expected : Set<Band> =
        [1..3]
        |> Set.ofList
        |> Set.map
            (fun b ->
                [1..3]
                |> Set.ofList
                |> Set.map
                    (fun _ ->
                        b |> BBand))
        |> Set.unionMany

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Get band rows``() = 
    let p = TPuzzleMap defaultPuzzleSpec :> PuzzleMap

    let actual : Set<Row> = p.bandRows.Get (2 |> BBand)

    let expected : Set<Row> =
        [4..6]
        |> Set.ofList
        |> Set.map
            (fun r ->
                r |> RRow)

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Get box for a cell``() =
    let p = TPuzzleMap defaultPuzzleSpec :> PuzzleMap

    let cells : Set<Cell> =
        [1..9]
        |> Set.ofList
        |> Set.map
            (fun r ->
                { Cell.col = 5 |> CColumn
                  row = r |> RRow })

    let actual : Set<Box> =
        cells
        |> Set.map p.cellBox.Get

    let expected : Set<Box> =
        [1..3]
        |> Set.ofList
        |> Set.map
            (fun b ->
                [1..3]
                |> Set.ofList
                |> Set.map
                    (fun _ ->
                        { Box.stack = 2 |> SStack
                          band = b |> BBand }))
        |> Set.unionMany

    Assert.AreEqual(expected, actual)

