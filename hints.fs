#light

module hints

open System
open System.Text

open sudoku
open tactics


type HiddenSingle = {
    cell : Cell
    symbol : Symbol
    house : House
}

