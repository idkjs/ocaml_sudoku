open Smap
open Sudoku

type puzzleMap =
    {
        columns : columns;
        rows : rows;
        cells : cells;
        stacks : stack list;
        bands : band list;
        boxes : box list;
        houses : house list;
        (* for a column, return the cells in it *)
        columnCells : (column * cells) list;
        (* for a row, return the cells in it *)
        rowCells : (row * cells) list;
        (* for a column, which stack is it in? *)
        columnStack : (column * stack) list;
        (* for a stack, return the columns in it *)
        stackColumns : (stack * column list) list;
        (* for a row, which band is it in? *)
        rowBand : (row * band) list;
        (* for a band, return the rows in it *)
        bandRows : (band * row list) list;
        (* for a cell, which box is it in? *)
        cellBox : (cell * box) list;
        (* for a box, return the cells in it *)
        boxCells : (box * cells) list;
        (* for a house, return the cells in it *)
        houseCells : (house * cells) list;
        cellHouseCells : (cell * cells) list;
        housesCells : houses -> cells;
        houseCellCandidateReductions : house -> cellCandidates -> candidateReduction list;

        //abstract member houseCellCandidates : (house, cellCandidates) list
    }

val tPuzzleMap : puzzleShape -> puzzleMap
