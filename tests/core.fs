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
    let p = tPuzzleMap defaultPuzzleSpec :> puzzleMap

    let actual : Set<column> = p.columns

    let expected : Set<column> =
        [1..9]
        |> Set.ofList
        |> Set.map
            (fun c ->
                c |> CColumn)

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Can make rows``() = 
    let p = tPuzzleMap defaultPuzzleSpec :> puzzleMap

    let actual : Set<row> = p.rows

    let expected : Set<row> =
        [1..9]
        |> Set.ofList
        |> Set.map
            (fun r ->
                r |> RRow)

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Can make cells``() = 
    let p = tPuzzleMap defaultPuzzleSpec :> puzzleMap

    let actual : Set<cell> = p.cells

    let expected : Set<cell> =
        [1..9]
        |> Set.ofList
        |> Set.map
            (fun r ->
                [1..9]
                |> Set.ofList
                |> Set.map
                    (fun c ->
                        { cell.col = c |> CColumn
                          row = r |> RRow }))
        |> Set.unionMany

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Can make stacks``() = 
    let p = tPuzzleMap twoByFourPuzzleSpec :> puzzleMap

    let actual : Set<stack> = p.stacks

    let expected : Set<stack> =
        [1..4]
        |> Set.ofList
        |> Set.map
            (fun s ->
                s |> SStack)

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Can make bands``() = 
    let p = tPuzzleMap twoByFourPuzzleSpec :> puzzleMap

    let actual : Set<band> = p.bands

    let expected : Set<band> =
        [1..2]
        |> Set.ofList
        |> Set.map
            (fun b ->
                b |> BBand )

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Can make boxes``() = 
    let p = tPuzzleMap twoByFourPuzzleSpec :> puzzleMap

    let actual : Set<box> = p.boxes

    let expected : Set<box> =
        [1..2]
        |> Set.ofList
        |> Set.map
            (fun b ->
                [1..4]
                |> Set.ofList
                |> Set.map
                    (fun s ->
                        { box.stack = s |> SStack
                          band = b |> BBand }))
        |> Set.unionMany

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Can make houses``() = 
    let p = tPuzzleMap twoByFourPuzzleSpec :> puzzleMap

    let actual : Set<house> = p.houses

    let expectedColumns : Set<house> =
        [1..9]
        |> Set.ofList
        |> Set.map
            (fun c ->
                c |> CColumn)
        |> Set.map HColumn

    let expectedRows : Set<house> =
        [1..9]
        |> Set.ofList
        |> Set.map
            (fun r ->
                r |> RRow)
        |> Set.map HRow

    let expectedBoxes : Set<house> =
        [1..2]
        |> Set.ofList
        |> Set.map
            (fun b ->
                [1..4]
                |> Set.ofList
                |> Set.map
                    (fun s ->
                        { box.stack = s |> SStack
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
    let p = tPuzzleMap defaultPuzzleSpec :> puzzleMap

    let actual : Set<cell> = p.columnCells.Get (2 |> CColumn)

    let expected : Set<cell> =
        [1..9]
        |> Set.ofList
        |> Set.map
            (fun r ->
                { cell.col = 2 |> CColumn
                  row = r |> RRow })

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Get row cells``() = 
    let p = tPuzzleMap defaultPuzzleSpec :> puzzleMap

    let actual : Set<cell> = p.rowCells.Get (7 |> RRow)

    let expected : Set<cell> =
        [1..9]
        |> Set.ofList
        |> Set.map
            (fun c ->
                { cell.col = c |> CColumn
                  row = 7 |> RRow })

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Get stack for a column``() =
    let p = tPuzzleMap defaultPuzzleSpec :> puzzleMap

    let columns : Set<column> = p.columns

    let actual : Set<stack> =
        columns
        |> Set.map p.columnStack.Get

    let expected : Set<stack> =
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
    let p = tPuzzleMap defaultPuzzleSpec :> puzzleMap

    let actual : Set<column> = p.stackColumns.Get (2 |> SStack)

    let expected : Set<column> =
        [4..6]
        |> Set.ofList
        |> Set.map
            (fun c ->
                c |> CColumn)

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Get band for a row``() =
    let p = tPuzzleMap defaultPuzzleSpec :> puzzleMap

    let rows : Set<row> = p.rows

    let actual : Set<band> =
        rows
        |> Set.map p.rowBand.Get

    let expected : Set<band> =
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
    let p = tPuzzleMap defaultPuzzleSpec :> puzzleMap

    let actual : Set<row> = p.bandRows.Get (2 |> BBand)

    let expected : Set<row> =
        [4..6]
        |> Set.ofList
        |> Set.map
            (fun r ->
                r |> RRow)

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Get box for a cell``() =
    let p = tPuzzleMap defaultPuzzleSpec :> puzzleMap

    let cells : Set<cell> =
        [1..9]
        |> Set.ofList
        |> Set.map
            (fun r ->
                { cell.col = 5 |> CColumn
                  row = r |> RRow })

    let actual : Set<box> =
        cells
        |> Set.map p.cellBox.Get

    let expected : Set<box> =
        [1..3]
        |> Set.ofList
        |> Set.map
            (fun b ->
                [1..3]
                |> Set.ofList
                |> Set.map
                    (fun _ ->
                        { box.stack = 2 |> SStack
                          band = b |> BBand }))
        |> Set.unionMany

    Assert.AreEqual(expected, actual)

