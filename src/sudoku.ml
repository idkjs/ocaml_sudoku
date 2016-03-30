
(* A sudoku is a square grid of size... *)
type size = int

(* containing columns... *)
type column = 
    | CColumn of int

module Column = struct
    let make (i : int) : column =
        i |> CColumn

    let comparer (CColumn c1 : column) (CColumn c2 : column) : int =
        if c1 < c2 then -1
        else if c1 = c2 then 0
        else 1

    let to_string (CColumn c : column) : string =
        Printf.sprintf "c%d" c
end

type columns = CColumns of column list

module Columns = struct

    let count (CColumns s : columns) : int = List.length s

    let drop (n : int) (CColumns columns : columns) : columns = Sset.drop n columns |> CColumns

    let ofList (s : column list) : columns = Sset.setify Column.comparer s |> CColumns

    let map (map : column -> 'b) (CColumns s : columns) : 'b list = List.map map s

    let mapi (map : int -> column -> 'b) (CColumns s : columns) : 'b list = List.mapi map s

    let toList (CColumns s : columns) : column list = s

    let list_to_string (s : column list) : string =
        s
        |> List.map Column.to_string
        |> String.concat ","

    let to_string (CColumns s : columns) : string = list_to_string s

    let union (CColumns s : columns) (CColumns s' : columns) : columns = Sset.union Column.comparer s s' |> CColumns
end

(* ... by rows *)
type row = 
    | RRow of int

module Row = struct
    let make (i : int) : row =
        i |> RRow

    let comparer (RRow r1 : row) (RRow r2 : row) : int =
        if r1 < r2 then -1
        else if r1 = r2 then 0
        else 1

    let to_string (RRow r : row) : string =
        Printf.sprintf "r%d" r
end

type rows = CRows of row list

module Rows = struct

    let count (CRows s : rows) : int = List.length s

    let drop (n : int) (CRows rows : rows) : rows = Sset.drop n rows |> CRows

    let ofList (s : row list) : rows = Sset.setify Row.comparer s |> CRows

    let map (map : row -> 'b) (CRows s : rows) : 'b list = List.map map s

    let mapi (map : int -> row -> 'b) (CRows s : rows) : 'b list = List.mapi map s

    let toList (CRows s : rows) : row list = s

    let list_to_string (s : row list) : string =
        s
        |> List.map Row.to_string
        |> String.concat ","

    let to_string (CRows s : rows) : string = list_to_string s

    let union (CRows s : rows) (CRows s' : rows) : rows = Sset.union Row.comparer s s' |> CRows
end

(* Each cell is identified by (col, row) *)
type cell = 
    { col : column;
      row : row }

module Cell = struct
    let make (c : column) (r : row) : cell =
        { cell.col = c;
          row = r }

    let comparer ({ col = CColumn c1; row = RRow r1} : cell) ({ col = CColumn c2; row = RRow r2} : cell) : int =
        if r1 < r2 then -1
        else if r1 = r2 then
            if c1 < c2 then -1
            else if c1 = c2 then 0
            else 1
        else 1

    let to_string ({col = CColumn c; row = RRow r} : cell) : string =
        Printf.sprintf "r%dc%d" r c
end

type cells = CCells of cell list

module Cells = struct

    let choose (map : cell -> 'b option) (CCells s : cells) : 'b list = List.choose map s

    let contains (d : cell) (CCells s : cells) : bool = Sset.contains<cell> Cell.comparer d s

    let count (CCells s : cells) : int = List.length s

    let difference (CCells s : cells) (CCells s' : cells) : cells = Sset.subtract Cell.comparer s s' |> CCells

    let filter (predicate : cell -> bool) (CCells s : cells) : cells = List.filter predicate s |> CCells

    let map (map : cell -> 'b) (CCells s : cells) : 'b list = List.map map s

    let ofList (as' : cell list) : cells = Sset.setify Cell.comparer as' |> CCells

    let remove (d : cell) (CCells s : cells) : cells = Sset.remove<cell> Cell.comparer d s |> CCells

    let singleton (d : cell) : cells = [ d ] |> CCells

    let toList (CCells s : cells) : cell list = s

    let list_to_string (s : cell list) : string =
        s
        |> List.map Cell.to_string
        |> String.concat ","

    let to_string (CCells s : cells) : string = list_to_string s

    let union (CCells s : cells) (CCells s' : cells) : cells = Sset.union Cell.comparer s s' |> CCells

    let unionManyList (ss : cells list) : cells =
        let tss = List.map toList ss in
        Sset.unions Cell.comparer tss |> CCells
end

(* The grid is divided into boxes,
 these do not have to be square, but they are
 all the same size and cover the grid
 A column of vertical boxes is a stack *)
type stack = 
    | SStack of int

module Stack = struct
    let make (i : int) : stack =
        i |> SStack

    let comparer (SStack s1 : stack) (SStack s2 : stack) : int =
        if s1 < s2 then -1
        else if s1 = s2 then 0
        else 1

    let to_string (SStack s : stack) : string =
        Printf.sprintf "stk%d" s
end

module Stacks = struct
    let list_to_string (s : stack list) : string =
        s
        |> List.map Stack.to_string
        |> String.concat ","
end

type boxWidth = int

(* A row of horizontal boxes is a band *)
type band = 
    | BBand of int

module Band = struct
    let make (i : int) : band =
        i |> BBand

    let comparer (BBand b1 : band) (BBand b2 : band) : int =
        if b1 < b2 then -1
        else if b1 = b2 then 0
        else 1

    let to_string (BBand b : band) : string =
        Printf.sprintf "bnd%d" b
end

module Bands = struct
    let list_to_string (s : band list) : string =
        s
        |> List.map Band.to_string
        |> String.concat ","
end

type boxHeight = int

(* A box is the intersection of a stack and a band *)
type box = 
    { stack : stack;
      band : band }

module Box = struct
    let make (s : stack) (b : band) : box =
        { box.stack = s;
          band = b }

    let comparer ({ stack = SStack s1; band = BBand b1} : box) ({ stack = SStack s2; band = BBand b2} : box) : int =
        if b1 < b2 then -1
        else if b1 = b2 then
            if s1 < s2 then -1
            else if s1 = s2 then 0
            else 1
        else 1

    let to_string ({stack = SStack s; band = BBand b} : box) : string =
        Printf.sprintf "bnd%dstk%d" b s
end

module Boxes = struct
    let list_to_string (s : box list) : string =
        s
        |> List.map Box.to_string
        |> String.concat ","
end

(* The columns and rows are collectively called lines *)
type line = 
    | LColumn of column
    | LRow of row

(* The columns, rows and boxes are collectively called houses *)
type house = 
    | HColumn of column
    | HRow of row
    | HBox of box

module House = struct
    let comparer (h1 : house) (h2 : house) : int =
        match h1, h2 with
        | HColumn c1, HColumn c2 -> Column.comparer c1 c2
        | HColumn _, HRow _ -> -1
        | HColumn _, HBox _ -> -1
        | HRow _, HColumn _ -> 1
        | HRow r1, HRow r2 -> Row.comparer r1 r2
        | HRow _, HBox _ -> -1
        | HBox _, HColumn _ -> 1
        | HBox _, HRow _ -> 1
        | HBox b1, HBox b2 -> Box.comparer b1 b2

    let to_string (house : house) : string =
        match house with
        | HColumn column -> Column.to_string column
        | HRow row -> Row.to_string row
        | HBox box -> Box.to_string box
end

type houses = CHouses of house list

module Houses = struct

    let drop (n : int) (CHouses houses : houses) : houses = Sset.drop n houses |> CHouses

    let empty : houses = [] |> CHouses

    let map (map : house -> 'b) (CHouses s : houses) : 'b list = List.map map s

    let mapi (map : int -> house -> 'b) (CHouses s : houses) : 'b list = List.mapi map s

    let ofList (as' : house list) : houses = Sset.setify House.comparer as' |> CHouses

    let singleton (d : house) : houses = [ d ] |> CHouses

    let toList (CHouses houses : houses) : house list = houses

    let list_to_string (s : house list) : string =
        s
        |> List.map House.to_string
        |> String.concat ","

    let to_string (CHouses houses : houses) : string =
        list_to_string houses
end

(* Each cell in the grid contains a Digit, usually numbers 1..9 *)
type digit = 
    | Digit of char

module Digit = struct
    let make i = (char) i + '0' |> Digit

    let comparer (Digit d1 : digit) (Digit d2 : digit) : int =
        if d1 < d2 then -1
        else if d1 = d2 then 0
        else 1

    let to_string (Digit s : digit) : string =
        (string) s
end

type digits = CDigits of digit list

module Digits = struct
    let contains (d : digit) (CDigits s : digits) : bool = Sset.contains<digit> Digit.comparer d s

    let count (CDigits s : digits) : int = List.length s

    let difference (CDigits s : digits) (CDigits s' : digits) : digits = Sset.subtract Digit.comparer s s' |> CDigits

    let empty : digits = [] |> CDigits

    let filter (predicate : digit -> bool) (CDigits s : digits) : digits = List.filter predicate s |> CDigits

    let first (CDigits d : digits) : digit = 
        match d with
        | h :: _ -> h
        | [] -> failwith "Not empty"

    let intersect (CDigits s : digits) (CDigits s' : digits) : digits = Sset.intersect Digit.comparer s s' |> CDigits

    let isSubset (CDigits s : digits) (CDigits s' : digits) : bool = Sset.isSubset s s'

    let nth (CDigits s : digits) (i : int) : digit = List.nth s i

    let ofList (as' : digit list) : digits = Sset.setify Digit.comparer as' |> CDigits

    let remove (d : digit) (CDigits s : digits) : digits = Sset.remove Digit.comparer d s |> CDigits

    let singleton (d : digit) : digits = [ d ] |> CDigits

    let toList (CDigits s : digits) : digit list = s

    let union (CDigits s : digits) (CDigits s' : digits) : digits = Sset.union Digit.comparer s s' |> CDigits

    let unionManyList (ss : digits list) : digits =
        let tss = List.map toList ss in
        Sset.unions Digit.comparer tss |> CDigits

    let list_to_string (s : digit list) : string =
        s
        |> List.map Digit.to_string
        |> String.concat ","

    let to_string (CDigits digits : digits) : string = list_to_string digits
end

(* A sudoku is defined by the overall grid size (it is always square)
 which is the same as the Digits in the alphabet
 and also by the width and height of the boxes *)
type puzzleShape = 
    { size : size;
      boxWidth : boxWidth;
      boxHeight : boxHeight;
      alphabet : digits }

(*let rec range i j = if i > j then [] else i :: (range (i+1) j)*)

module PuzzleShape = struct
    let default' : puzzleShape = 
        { size = 9;
          boxWidth = 3;
          boxHeight = 3;
          alphabet =
            [1..9]
            |> List.map Digit.make
            |> Digits.ofList
            }
end

(* Whilst working to a solution each cell in the grid
 that doesn't have a Digit is filled with candidates
 Candidates are possible Digits *)
type cellContents = 
    | BigNumber of digit
    | PencilMarks of digits

(* Working towards a solution we take one of the following actions:
 Sset the cell to have a Digit *)
type value = 
    { cell : cell;
      digit : digit }

module Value = struct
    let make (cell : cell) (digit : digit) : value = 
        { value.cell = cell;
          digit = digit }

    let to_string ({ cell = cell; digit = digit} : value) : string =
        Printf.sprintf "%s=%s" (Cell.to_string cell) (Digit.to_string digit)
end

(* A candidate is a digit in a cell, which is still a pencilmark *)
type candidate = 
    { cell : cell;
      digit : digit }

module Candidate = struct
    let make (cell : cell) (digit : digit) : candidate =
        { candidate.cell = cell;
          digit = digit }

    let to_string ({ cell = cell; digit = digit} : candidate) : string =
        Printf.sprintf "(%s)%s" (Cell.to_string cell) (Digit.to_string digit)
end

type candidateReduction = 
    { cell : cell;
      candidates : digits }

module CandidateReduction = struct
    let make (cell : cell) (digits : digits) : candidateReduction =
        { candidateReduction.cell = cell;
          candidates = digits }

    let to_string ({ cell = cell; candidates = digits} : candidateReduction) : string =
        Printf.sprintf "Cell %s, Candidates %s" (Cell.to_string cell) (Digits.to_string digits)
end

module CandidateReductions = struct
    let to_string (s : candidateReduction list) : string =
        s
        |> List.map CandidateReduction.to_string
        |> String.concat ","
end

(* Working towards a solution we take one of the following actions:
 Sset the cell to have a Digit
 or remove a candidate *)
 [<NoComparisonAttribute>]
type action =
    | Load of string
    | LoadEliminate
    | Placement of value
    | Eliminate of candidate

module Action = struct
    let to_string (action : action) : string =
        match action with
        | Load sudoku -> Printf.sprintf "Load:%s" sudoku
        | LoadEliminate  -> "Load"
        | Placement a -> Printf.sprintf "%s=%s" (Cell.to_string a.cell) (Digit.to_string a.digit)
        | Eliminate candidate -> Printf.sprintf "%s<>%s" (Cell.to_string candidate.cell) (Digit.to_string candidate.digit)
end

type given = Given of (cell * digit option) list

module Given = struct
    let get (Given l:given) (k:cell) : digit option = Smap.get l k
end

type current = Current of (cell * cellContents) list

module Current = struct
    let get (Current l:current) (k:cell) : cellContents = Smap.get l k
end

(* for a cell, return a set of candidates *)
type cellCandidates = CellCandidates of (cell * digits) list

module CellCandidates = struct
    let get (CellCandidates l:cellCandidates) (k:cell) : digits = Smap.get l k
end

[<NoComparisonAttribute>]
type solution = 
    { given : given;
      current : current;
      steps : action list }

module Solution = struct
    let givenToCurrent (cells : cells) (given : given) (alphabet : digits) : current =
        let makeCellContents (cell : cell) : cellContents =
            let dop = Given.get given cell in
            match dop with
            | Some digit -> BigNumber digit
            | None -> PencilMarks alphabet
            in

        cells
        |> Cells.toList
        |> List.map (fun a -> (a, makeCellContents a))
        |> Current

    let currentCellCandidates (cells : cells) (current : current) : cellCandidates =
        let getCandidateEntries (cell : cell) : digits =
            let cellContents = Current.get current cell in
            match cellContents with
            | BigNumber _ -> Digits.empty
            | PencilMarks s -> s
            in

        cells
        |> Cells.toList
        |> List.map (fun a -> (a, getCandidateEntries a))
        |> CellCandidates
end
