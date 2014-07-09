module clearCandidate

open System
open System.Text

open sudoku
open format
open console

let clearCandidateToString (cc:ClearCandidate) =
    String.Format("ClearCandidate: {0} = {1}", formatCell cc.cell, formatSymbol cc.value)
