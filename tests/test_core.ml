module test_core

open Smap
open Sudoku
open Puzzlemap

open NUnit.Framework

let twoByFourPuzzleSpec =
    { size = 8;
      boxWidth = 2;
      boxHeight = 4;
      alphabet = 
          [ for i in 1..8 -> (char) i + '0' |> Digit ] }

[<Test>]
let ``Can make columns``() =
    let p = tPuzzleMap defaultPuzzleSpec in

    let expected =
        [1..9]
        |> List.map CColumn
        in

    Assert.AreEqual(expected, p.columns)

[<Test>]
let ``Can make rows``() = 
    let p = tPuzzleMap defaultPuzzleSpec in

    let expected =
        [1..9]
        |> List.map RRow
        in

    Assert.AreEqual(expected, p.rows)

[<Test>]
let ``Can make cells``() = 
    let p = tPuzzleMap defaultPuzzleSpec in

    let expected =
        [1..9]
        |> List.map
            (fun r ->
                [1..9]
                |> List.map
                    (fun c -> makeCell (c |> CColumn) (r |> RRow)))
        |> List.concat
        in

    Assert.AreEqual(expected, p.cells)

[<Test>]
let ``Can make stacks``() = 
    let p = tPuzzleMap twoByFourPuzzleSpec in

    let expected =
        [1..4]
        |> List.map SStack
        in

    Assert.AreEqual(expected, p.stacks)

[<Test>]
let ``Can make bands``() = 
    let p = tPuzzleMap twoByFourPuzzleSpec in

    let expected =
        [1..2]
        |> List.map BBand
        in

    Assert.AreEqual(expected, p.bands)

[<Test>]
let ``Can make boxes``() = 
    let p = tPuzzleMap twoByFourPuzzleSpec in

    let expected =
        [1..2]
        |> List.map
            (fun b ->
                [1..4]
                |> List.map
                    (fun s -> makeBox (s |> SStack) (b |> BBand)))
        |> List.concat
        in

    Assert.AreEqual(expected, p.boxes)

[<Test>]
let ``Can make houses``() = 
    let p = tPuzzleMap twoByFourPuzzleSpec in

    let expectedColumns =
        [1..8]
        |> List.map CColumn
        |> List.map HColumn
        in

    let expectedRows =
        [1..8]
        |> List.map RRow
        |> List.map HRow
        in

    let expectedBoxes =
        [1..2]
        |> List.map
            (fun b ->
                [1..4]
                |> List.map
                    (fun s -> makeBox (s |> SStack) (b |> BBand)))
        |> List.concat
        |> List.map HBox
        in

    let expected =
        [ expectedColumns; expectedRows; expectedBoxes]
        |> List.concat
        in

    Assert.AreEqual(expected, p.houses)

[<Test>]
let ``Get column cells``() = 
    let p = tPuzzleMap defaultPuzzleSpec in

    let actual = SMap.get p.columnCells (2 |> CColumn) in

    let expected =
        [1..9]
        |> List.map
            (fun r -> makeCell (2 |> CColumn) (r |> RRow))
        in

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Get row cells``() = 
    let p = tPuzzleMap defaultPuzzleSpec in

    let actual = SMap.get p.rowCells (7 |> RRow) in

    let expected =
        [1..9]
        |> List.map
            (fun c -> makeCell (c |> CColumn) (7 |> RRow))
        in

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Get stack for a column``() =
    let p = tPuzzleMap defaultPuzzleSpec in

    let actual =
        p.columns
        |> List.map (SMap.get p.columnStack)
        in

    let expected =
        [1..3]
        |> List.map
            (fun s ->
                [1..3]
                |> List.map (fun _ -> s |> SStack))
        |> List.concat
        in

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Get stack columns``() = 
    let p = tPuzzleMap defaultPuzzleSpec in

    let actual = SMap.get p.stackColumns (2 |> SStack) in

    let expected =
        [4..6]
        |> List.map CColumn
        in

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Get band for a row``() =
    let p = tPuzzleMap defaultPuzzleSpec in

    let actual =
        p.rows
        |> List.map (SMap.get p.rowBand)
        in

    let expected =
        [1..3]
        |> List.map
            (fun b ->
                [1..3]
                |> List.map (fun _ -> b |> BBand))
        |> List.concat
        in

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Get band rows``() = 
    let p = tPuzzleMap defaultPuzzleSpec in

    let actual = SMap.get p.bandRows (2 |> BBand) in

    let expected =
        [4..6]
        |> List.map RRow
        in

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Get box for a cell``() =
    let p = tPuzzleMap defaultPuzzleSpec in

    let cells =
        [1..9]
        |> List.map
            (fun r -> makeCell (5 |> CColumn) (r |> RRow))
        in

    let actual =
        cells
        |> List.map (SMap.get p.cellBox)
        in

    let expected =
        [1..3]
        |> List.map
            (fun b ->
                [1..3]
                |> List.map
                    (fun _ -> makeBox (2 |> SStack) (b |> BBand)))
        |> List.concat
        in

    Assert.AreEqual(expected, actual)
