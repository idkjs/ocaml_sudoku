module core.puzzlemap

open sudoku

val konst : 'a -> 'b -> 'a

val candidateToSymbol : Candidate -> Symbol

val symbolToCandidate : Symbol -> Candidate

[<NoEquality; NoComparison>]
type PuzzleMaps = 
    { columns : Column list
      rows : Row list
      cells : Cell list

      cellColumn : Cell -> Column
      columnCells : Column -> Cell list
      cellRow : Cell -> Row
      rowCells : Row -> Cell list
      cellBox : Cell -> Box
      boxCells : Box -> Cell list

      stacks : Stack list
      bands : Band list
      boxes : Box list

      columnStack : Column -> Stack
      stackColumns : Stack -> Column list
      rowBand : Row -> Band
      bandRows : Band -> Row list }

val makePuzzleMaps : Puzzle -> PuzzleMaps

val getHouseCells : PuzzleMaps -> House -> Set<Cell>

val allHouses : PuzzleMaps -> House list

// For a cell, return all the cell symbols for its containing column
val rowCellsForCell : PuzzleMaps -> (Cell -> Cell list)

// For a cell, return all the cell symbols for its containing row
val colCellsForCell : PuzzleMaps -> (Cell -> Cell list)

// For a cell, return all the cell symbols for its containing box
val boxCellsForCell : PuzzleMaps -> (Cell -> Cell list)

val allHouseCells : PuzzleMaps -> Cell -> Set<Cell>

val houseCellsForCell : PuzzleMaps -> Cell -> (Cell -> 'a option) -> Set<'a>

val setCellValueModelEffect : PuzzleMaps -> SetCellValue -> (Cell -> Set<Candidate>) -> Set<CandidateReduction>

val setCellCandidateReductions : PuzzleMaps -> SetCellValue -> (Cell -> Set<Candidate>) -> Set<CandidateReduction>
