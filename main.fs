#light

module main

open System
open System.Text

open sudoku
open console
open tactics

// Update the set of candidates
let updateCandidates (entryLookup:EntryLookup) houseCells (cells:Cell list) alphabet =
    let grid = new SolutionGrid()

    let symbolLookup = entryLookup >> entryToAlphabet

    List.iter (
        fun cell ->
            let entry =
                let d = entryLookup cell
                match d with
                | Given(_) -> d
                | Set(_) -> d
                | Candidates(_) ->
                    let c = candidateSymbols cell symbolLookup houseCells alphabet
                    Candidates(c)
            grid.Add(cell, entry)
    ) cells

    grid

let beginSolution (symbolLookup : SymbolLookup) houseCells (cells:Cell list) (puzzleSpec : PuzzleSpec) =
    let solutionGrid = new SolutionGrid()

    List.iter (
        fun cell ->
            let entry =
                match symbolLookup cell with
                | Some(e) -> Given(e)
                | None ->
                    let c = 
                        candidateSymbols cell symbolLookup houseCells puzzleSpec.alphabet
                    Candidates(c)
            solutionGrid.Add(cell, entry)
    ) cells


    let firstStep = { grid = (solutionGridCellLookup solutionGrid); action = Load }

    let solution = { spec = puzzleSpec; puzzle = symbolLookup; steps = [firstStep] }

    solution




let print_step (write:ConsoleCharWriter) eol step stacks stackColumns bands bandRows =
    let grid = step.grid
    let writeEntry c acc = c :: acc
    List.iter write (print_long (grid >> entryToConsole >> writeEntry) eol stacks stackColumns bands bandRows [])

    match step.action with
    | Load -> write (CStr "Load")
    | SetValue({col = col; row = row}, Symbol s) -> write (CStr (String.Format("SetValue: ({0}, {1}) = {2}", (int)col.col, (int)row.row, s)))
    | ClearCandidate({col = col; row = row}, Symbol s) -> write (CStr (String.Format("ClearCandidate: ({0}, {1}) = {2}", (int)col.col, (int)row.row, s)))

    write NL

let print_last eol solution (stacks:Stack list) stackColumns (bands:Band list) bandRows =
    let lastStep = solution.steps.Head
    print_step ConsoleWriteChar eol lastStep stacks stackColumns bands bandRows


let getInput (prompt:string) =
    Console.Write prompt
    Console.ReadLine()

let readlines = Seq.initInfinite (fun _ -> getInput(">"))

let findCell (c:int<col>) (r:int<row>) (cells:Cell list) =
    List.tryFind (
        fun cell -> cell.col.col = c && cell.row.row = r
        ) cells

let ui_set (item:string) gridSize alphabet (lastGrid:EntryLookup) houseCells (cells:Cell list) =
    let terms = item.Split(' ')
    if terms.Length <> 4 then
        Console.WriteLine "Expect set <col> <row> <val>"
        None
    else
        let (didParseCol, col) = Int32.TryParse terms.[1]
        let (didParseRow, row) = Int32.TryParse terms.[2]
        let value = terms.[3]
        if didParseCol = false || didParseRow = false then
            Console.WriteLine "Expect set <col> <row> <val>"
            None
        else if col < 1 || col > gridSize || row < 1 || row > gridSize then
            Console.WriteLine ("Values must be between 1 and {0}", gridSize)
            None
        else if value.Length <> 1 then
            Console.WriteLine ("Expect a single digit, not {0}", value)
            None
        else
            let newValueOpt = charToAlphabet alphabet (value.Chars 0) 
            match newValueOpt with
            | Some(newValue) ->
                let cellOpt = findCell (col * 1<col>) (row * 1<row>) cells
                match cellOpt with
                | Some(cell) ->
                    let c = lastGrid(cell)
                    match c with
                    | Given(Symbol s) ->
                        Console.WriteLine ("Cell <{0}, {1}> has given value {2}", col, row, s)
                        None
                    | Set(Symbol s) ->
                        Console.WriteLine ("Cell <{0}, {1}> has been set value {2}", col, row, s)
                        None
                    | Candidates(_) ->
                        let lookup cell2 =
                            if cell = cell2 then
                                Set(newValue)
                            else
                                lastGrid(cell2)

                        let updatedGrid = updateCandidates lookup houseCells cells alphabet

                        let action = SetValue (cell, newValue)

                        let step = { grid = (solutionGridCellLookup updatedGrid); action = action }

                        Some step
                | None ->
                    Console.WriteLine("Not a cell")
                    None
            | None ->
                Console.WriteLine ("Not a valid digit")
                None


let getCandidateEntries = function
    | Given(_) -> Set.empty
    | Set(_) -> Set.empty
    | Candidates(s) -> s

let hiddenSinglesPerHouse alphabet (entryLookup:EntryLookup) (house:House) (cells:Cell list) =

    let candidates = cells |> List.map (fun cell -> (getCandidateEntries (entryLookup cell), cell))

    let hs =
        List.map (
            fun a ->
                let c = List.filter (fun (b, cell) -> Set.contains a b) candidates
                let cells = List.map snd c
                (cells, a))
            alphabet

    let hhs = List.filter (fun (a,b) -> List.length a = 1) hs

    let hhhs =
        List.map (
            fun (cells:Cell list, j) -> (List.head cells, j, house))
            hhs

    hhhs

// Hidden singles is when a house has only one possible square for a candidate
let hiddenSingles alphabet (entryLookup:EntryLookup) (columns:Column list) (rows:Row list) (boxes:Box list) (houseCells:HouseCells) =
    for c in columns do
        let house = Column c
        let hhhs = c |> houseCells.columnCells |> hiddenSinglesPerHouse alphabet entryLookup house

        List.iter (
            fun (cell, Symbol j, d) ->
                Console.WriteLine ("Column {0}, Value {1}, Cell {2}, {3}", c.col, j, (int)cell.col.col, (int)cell.row.row))
            hhhs

    for r in rows do
        let house = Row r
        let hhhs = r |> houseCells.rowCells |> hiddenSinglesPerHouse alphabet entryLookup house

        List.iter (
            fun (cell, Symbol j, d) ->
                Console.WriteLine ("Row {0}, Value {1}, Cell {2}, {3}", r.row, j, (int)cell.col.col, (int)cell.row.row))
            hhhs

    for b in boxes do
        let house = Box b
        let hhhs = b |> houseCells.boxCells |> hiddenSinglesPerHouse alphabet entryLookup house

        List.iter (
            fun (cell, Symbol j, d) ->
                Console.WriteLine ("Box {0} x {1}, Value {2}, Cell {3}, {4}", (int)b.stack.stack, (int)b.band.band, j, (int)cell.col.col, (int)cell.row.row))
            hhhs

let parse (item:string) gridSize alphabet eol solution (stacks:Stack list) (stackColumns:Stack -> Column list) (bands:Band list) (bandRows:Band->Row list) houseCells (cells:Cell list) (columns:Column list) (rows:Row list) (boxes:Box list) = 
    Console.WriteLine item

    if item = "print" then
        let lastStep = solution.steps.Head
        let grid = lastStep.grid

        let pf = print_full grid stacks stackColumns bands bandRows alphabet []
        List.iter ConsoleWriteChar pf

    else if item.StartsWith "set" then
        let lastStep = solution.steps.Head
        let lastGrid = lastStep.grid

        let set = ui_set item gridSize alphabet lastGrid houseCells cells
        match set with
        | Some step -> solution.steps <- step :: solution.steps
        | None -> Console.WriteLine("")

        print_last eol solution stacks stackColumns bands bandRows

    else if item = "hs" then
        let lastStep = solution.steps.Head
        let lastGrid = lastStep.grid

        hiddenSingles alphabet lastGrid columns rows boxes houseCells

let run gridSize alphabet eol solution (stacks:Stack list) stackColumns (bands:Band list) bandRows houseCells (cells:Cell list) (columns:Column list) (rows:Row list) (boxes:Box list) item =
    if item = "quit"
        then
            Some(item)
        else
            parse item gridSize alphabet eol solution stacks stackColumns bands bandRows houseCells cells columns rows boxes
            None


type Tactics = {
    stacks : Stack list
    bands : Band list

    columns : Column list
    rows : Row list
    boxes : Box list

    cells : Cell list

    stackColumns : Stack -> Column list
    bandRows : Band -> Row list

    columnCells : Column -> Cell list
    rowCells : Row -> Cell list
    boxCells : Box -> Cell list

    houseCells : HouseCells
}

let makeTactics (puzzleSpec : PuzzleSpec) =
    let (stacks, columns, columnStackLookup, stackColumnLookup) = eachStackColumns puzzleSpec.boxWidth puzzleSpec.gridSize
    let (bands, rows, rowBandLookup, bandRowLookup) = eachBandRows puzzleSpec.boxHeight puzzleSpec.gridSize
    let boxes = eachBox stacks bands
    let cells = allCells columns rows
    let (columnCells, cellColumnLookup) = makeContainerColumnCell columns cells
    let (rowCells, cellRowLookup) = makeContainerRowCell rows cells
    let (cellBoxLookup, boxCellLookup) = makeContainerBoxCell boxes cells columnStackLookup rowBandLookup

    let houseCells =
        {
            HouseCells.cellColumn = cellColumnLookup
            columnCells = columnCells
            cellRow = cellRowLookup
            rowCells = rowCells
            cellBox = cellBoxLookup
            boxCells = boxCellLookup
        }

    let tactics =
        {
            stacks = stacks
            columns = columns
            bands = bands
            rows = rows
            boxes = boxes
            cells = cells

            stackColumns = stackColumnLookup
            bandRows = bandRowLookup

            columnCells = columnCells
            rowCells = rowCells
            boxCells = boxCellLookup

            houseCells = houseCells
        }

    tactics


let mainWriter = ConsoleWriteChar

let repl (sudoku:string) (puzzleSpec : PuzzleSpec) =

    Console.WriteLine sudoku

    let alphabetisedLine = loadLine sudoku puzzleSpec.alphabet

    let tactics = makeTactics puzzleSpec

    let puzzleGrid = loadPuzzle alphabetisedLine tactics.cells

    let solution = beginSolution puzzleGrid tactics.houseCells tactics.cells puzzleSpec

    let writeEntry c acc = c :: acc

    let line = List.foldBack (puzzleGrid >> symbolOptionToConsoleChar >> writeEntry) tactics.cells [NL]
    List.iter mainWriter line
    mainWriter NL

    
    let eol acc = NL :: acc

    let rows = printRowOnOneLine (puzzleGrid >> symbolOptionToConsoleChar >> writeEntry) tactics.houseCells.rowCells eol tactics.rows []
    List.iter mainWriter rows

    List.iter ConsoleWriteChar (print_long (puzzleGrid >> symbolOptionToConsoleChar >> writeEntry) eol tactics.stacks tactics.stackColumns tactics.bands tactics.bandRows [])

    Seq.tryPick (run puzzleSpec.gridSize puzzleSpec.alphabet eol solution tactics.stacks tactics.stackColumns tactics.bands tactics.bandRows tactics.houseCells tactics.cells tactics.columns tactics.rows tactics.boxes) readlines |> ignore


let defaultPuzzleSpec = {
    boxWidth = 3
    boxHeight = 3
    gridSize = 9
    alphabet = [ for i in 1 .. 9 -> (char) i + '0' |> Symbol ]
}

// Input puzzle
let header1 = "1........2........3........4........5........6........7........8........9........"
let header2 = "123456789123456789123456789123456789123456789123456789123456789123456789123456789"
Console.WriteLine header1
Console.WriteLine header2


//let example = "410230000700580040000000020190000700380000016000008400000806005031050000000090800"
let example = "000105000140000670080002400063070010900000003010090520007200080026000035000409000"

repl example defaultPuzzleSpec

Console.WriteLine "bye"
