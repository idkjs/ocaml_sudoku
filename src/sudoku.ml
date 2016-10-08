(*F# open FSharp.Compatibility.OCaml F#*)

(* A sudoku is a square grid of size... *)
type size = int

(* containing columns... *)
type column = 
    | CColumn of int

module Column = struct
    let comparer (CColumn c1 : column) (CColumn c2 : column) : int =
        if c1 < c2 then -1
        else if c1 = c2 then 0
        else 1

    let make (i : int) : column =
        CColumn i

    let to_string (CColumn c : column) : string =
        Printf.sprintf "c%d" c
end

type columns =
    | CColumns of column list

module Columns = struct
    let make' (l : column list) : columns =
        CColumns l

    let count (CColumns cs : columns) : int =
        cs
        |> List.length

    let drop (n : int) (CColumns cs : columns) : columns =
        cs
        |> Sset.drop n
        |> make'

    let make (cs : column list) : columns =
        cs
        |> Sset.setify Column.comparer
        |> make'

    let map (map : column -> 'b) (CColumns cs : columns) : 'b list =
        cs
        |> List.map map

    let mapi (map : int -> column -> 'b) (CColumns cs : columns) : 'b list =
        cs
        |> List.mapi map

    let to_list (CColumns cs : columns) : column list =
        cs

    let list_to_string (cs : column list) : string =
        cs
        |> List.map Column.to_string
        |> String.concat ","

    let to_string (CColumns cs : columns) : string =
        cs
        |> list_to_string

    let union (CColumns cs : columns) (CColumns cs' : columns) : columns =
        Sset.union Column.comparer cs cs'
        |> make'
end

(* ... by rows *)
type row = 
    | RRow of int

module Row = struct
    let comparer (RRow r1 : row) (RRow r2 : row) : int =
        if r1 < r2 then -1
        else if r1 = r2 then 0
        else 1

    let make (i : int) : row = RRow i

    let to_string (RRow r : row) : string =
        Printf.sprintf "r%d" r
end

type rows = 
    | CRows of row list

module Rows = struct
    let make' (l : row list) : rows =
        CRows l

    let count (CRows rs : rows) : int =
        rs
        |> List.length

    let drop (n : int) (CRows rs : rows) : rows =
        rs
        |> Sset.drop n
        |> make'

    let make (rs : row list) : rows =
        rs
        |> Sset.setify Row.comparer
        |> make'

    let map (map : row -> 'b) (CRows rs : rows) : 'b list =
        rs
        |> List.map map

    let mapi (map : int -> row -> 'b) (CRows rs : rows) : 'b list =
        rs
        |> List.mapi map

    let to_list (CRows rs : rows) : row list =
        rs

    let list_to_string (rs : row list) : string =
        rs
        |> List.map Row.to_string
        |> String.concat ","

    let to_string (CRows rs : rows) : string =
        rs
        |> list_to_string

    let union (CRows rs : rows) (CRows rs' : rows) : rows =
        Sset.union Row.comparer rs rs'
        |> make'
end

(* Each cell is identified by (col, row) *)
type cell = 
    { col : column;
      row : row }

module Cell = struct
    let comparer ({ col = CColumn c1; row = RRow r1} : cell) ({ col = CColumn c2; row = RRow r2} : cell) : int =
        if r1 < r2 then -1
        else if r1 = r2 then
            if c1 < c2 then -1
            else if c1 = c2 then 0
            else 1
        else 1

    let make (c : column) (r : row) : cell =
        { col = c;
          row = r }

    let to_string ({col = CColumn c; row = RRow r} : cell) : string =
        Printf.sprintf "r%dc%d" r c
end

type cells =
    | CCells of cell list

module Cells = struct
    let make' (l : cell list) : cells =
        CCells l

    let choose (map : cell -> 'b option) (CCells cs : cells) : 'b list =
        cs
        |> Sset.choose map

    let contains (c : cell) (CCells cs : cells) : bool =
        cs
        |> Sset.contains Cell.comparer c

    let count (CCells cs : cells) : int =
        cs
        |> List.length

    let difference (CCells cs : cells) (CCells cs' : cells) : cells =
        Sset.subtract Cell.comparer cs cs'
        |> make'

    let filter (predicate : cell -> bool) (CCells cs : cells) : cells =
        cs
        |> List.filter predicate
        |> make'

    let map (map : cell -> 'b) (CCells cs : cells) : 'b list =
        cs
        |> List.map map

    let make (cs : cell list) : cells =
        cs
        |> Sset.setify Cell.comparer
        |> make'

    let remove (c : cell) (CCells cs : cells) : cells =
        cs
        |> Sset.remove Cell.comparer c
        |> make'

    let singleton (c : cell) : cells =
        [ c ]
        |> make'

    let to_list (CCells cs : cells) : cell list =
        cs

    let list_to_string (cs : cell list) : string =
        cs
        |> List.map Cell.to_string
        |> String.concat ","

    let to_string (CCells cs : cells) : string =
        cs
        |> list_to_string

    let union (CCells cs : cells) (CCells cs' : cells) : cells =
        Sset.union Cell.comparer cs cs'
        |> make'

    let union_many (cs : cells list) : cells =
        cs
        |> List.map to_list
        |> Sset.unions Cell.comparer
        |> make'
end

(* The grid is divided into boxes,
 these do not have to be square, but they are
 all the same size and cover the grid
 A column of vertical boxes is a stack *)
type stack = 
    | SStack of int

module Stack = struct
    let comparer (SStack s1 : stack) (SStack s2 : stack) : int =
        if s1 < s2 then -1
        else if s1 = s2 then 0
        else 1

    let make (i : int) : stack =
        SStack i

    let to_string (SStack s : stack) : string =
        Printf.sprintf "stk%d" s
end

module Stacks = struct
    let list_to_string (ss : stack list) : string =
        ss
        |> List.map Stack.to_string
        |> String.concat ","
end

type boxWidth = int

(* A row of horizontal boxes is a band *)
type band = 
    | BBand of int

module Band = struct
    let comparer (BBand b1 : band) (BBand b2 : band) : int =
        if b1 < b2 then -1
        else if b1 = b2 then 0
        else 1

    let make (i : int) : band =
        BBand i

    let to_string (BBand b : band) : string =
        Printf.sprintf "bnd%d" b
end

module Bands = struct
    let list_to_string (bs : band list) : string =
        bs
        |> List.map Band.to_string
        |> String.concat ","
end

type boxHeight = int

(* A box is the intersection of a stack and a band *)
type box = 
    { stack : stack;
      band : band }

module Box = struct
    let comparer ({ stack = SStack s1; band = BBand b1} : box) ({ stack = SStack s2; band = BBand b2} : box) : int =
        if b1 < b2 then -1
        else if b1 = b2 then
            if s1 < s2 then -1
            else if s1 = s2 then 0
            else 1
        else 1

    let make (s : stack) (b : band) : box =
        { stack = s;
          band = b }

    let to_string ({stack = SStack s; band = BBand b} : box) : string =
        Printf.sprintf "bnd%dstk%d" b s
end

module Boxes = struct
    let list_to_string (bs : box list) : string =
        bs
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

    let make_column (column : column) : house =
        HColumn column

    let make_row (row : row) : house =
        HRow row

    let make_box (box : box) : house =
        HBox box

    let to_string (house : house) : string =
        match house with
        | HColumn column -> Column.to_string column
        | HRow row -> Row.to_string row
        | HBox box -> Box.to_string box
end

type houses =
    | CHouses of house list

module Houses = struct
    let make' (l : house list) : houses =
        CHouses l

    let drop (n : int) (CHouses hs : houses) : houses =
        hs
        |> Sset.drop n
        |> make'

    let empty : houses =
        []
        |> make'

    let map (map : house -> 'b) (CHouses hs : houses) : 'b list =
        hs
        |> List.map map

    let mapi (map : int -> house -> 'b) (CHouses hs : houses) : 'b list =
        hs
        |> List.mapi map

    let make (hs : house list) : houses =
        hs
        |> Sset.setify House.comparer
        |> make'

    let singleton (h : house) : houses =
        [ h ]
        |> make'

    let to_list (CHouses hs : houses) : house list =
        hs

    let list_to_string (hs : house list) : string =
        hs
        |> List.map House.to_string
        |> String.concat ","

    let to_string (CHouses hs : houses) : string =
        hs
        |> list_to_string
end

(* Each cell in the grid contains a Digit, usually numbers 1..9 *)
type digit = 
    | Digit of char

module Digit = struct
    let comparer (Digit d1 : digit) (Digit d2 : digit) : int =
        if d1 < d2 then -1
        else if d1 = d2 then 0
        else 1

    let make (i : int) : digit =
        Digit (Char.chr (i + (Char.code '0')))

    let to_string (Digit s : digit) : string =
        String.make 1 s
end

type digits =
    | CDigits of digit list

module Digits = struct
    let make' (l : digit list) : digits =
        CDigits l

    let contains (d : digit) (CDigits ds : digits) : bool =
        ds
        |> Sset.contains Digit.comparer d

    let count (CDigits ds : digits) : int =
        ds
        |> List.length

    let difference (CDigits ds : digits) (CDigits ds' : digits) : digits =
        Sset.subtract Digit.comparer ds ds'
        |> make'

    let empty : digits =
        []
        |> make'

    let filter (predicate : digit -> bool) (CDigits ds : digits) : digits =
        ds
        |> List.filter predicate
        |> make'

    let first (CDigits ds : digits) : digit = 
        match ds with
        | d :: _ -> d
        | [] -> failwith "Not empty"

    let intersect (CDigits ds : digits) (CDigits ds' : digits) : digits =
        Sset.intersect Digit.comparer ds ds'
        |> make'

    let is_subset (CDigits ds : digits) (CDigits ds' : digits) : bool =
        Sset.subset Digit.comparer ds ds'

    let nth (CDigits ds : digits) (i : int) : digit =
        List.nth ds i

    let make (ds : digit list) : digits =
        ds
        |> Sset.setify Digit.comparer
        |> make'

    let remove (d : digit) (CDigits ds : digits) : digits = 
        ds
        |> Sset.remove Digit.comparer d
        |> make'

    let singleton (d : digit) : digits =
        [ d ]
        |> make'

    let to_list (CDigits ds : digits) : digit list =
        ds

    let union (CDigits ds : digits) (CDigits ds' : digits) : digits =
        Sset.union Digit.comparer ds ds'
        |> make'

    let union_many (ds : digits list) : digits =
        ds
        |> List.map to_list
        |> Sset.unions Digit.comparer
        |> make'

    let list_to_string (ds : digit list) : string =
        ds
        |> List.map Digit.to_string
        |> String.concat ","

    let to_string (CDigits ds : digits) : string =
        ds
        |> list_to_string
end

(* A sudoku is defined by the overall grid size (it is always square)
 which is the same as the Digits in the alphabet
 and also by the width and height of the boxes *)
type puzzleShape = 
    { size : size;
      boxWidth : boxWidth;
      boxHeight : boxHeight;
      alphabet : digits }

module PuzzleShape = struct
    let default' : puzzleShape = 
        { size = 9;
          boxWidth = 3;
          boxHeight = 3;
          alphabet =
            Sset.range 1 9
            |> List.map Digit.make
            |> Digits.make
            }
end

(* Whilst working to a solution each cell in the grid
 that doesn't have a Digit is filled with candidates
 Candidates are possible Digits *)
type cellContents = 
    | BigNumber of digit
    | PencilMarks of digits

module CellContents = struct
    let make_big_number (digit : digit) : cellContents =
        BigNumber digit

    let make_pencil_marks (digits : digits) : cellContents =
        PencilMarks digits
end

(* Working towards a solution we take one of the following actions:
 Sset the cell to have a Digit *)
type value = 
    { cell : cell;
      digit : digit }

module Value = struct
    let make (cell : cell) (digit : digit) : value = 
        { cell = cell;
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
        { cell = cell;
          digit = digit }

    let to_string ({ cell = cell; digit = digit} : candidate) : string =
        Printf.sprintf "(%s)%s" (Cell.to_string cell) (Digit.to_string digit)
end

type candidateReduction = 
    { cell : cell;
      candidates : digits }

module CandidateReduction = struct
    let make (cell : cell) (digits : digits) : candidateReduction =
        { cell = cell;
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

type given =
    | Given of (cell * digit option) list

module Given = struct
    let get (k : cell) (Given l : given) : digit option =
        Smap.get Cell.comparer k l
end

type current =
    | Current of (cell * cellContents) list

module Current = struct
    let get (k : cell) (Current l : current) : cellContents =
        Smap.get Cell.comparer k l

    let make (l : (cell * cellContents) list) : current =
        Current l
end

(* for a cell, return a set of candidates *)
type cellCandidates =
    | CellCandidates of (cell * digits) list

module CellCandidates = struct
    let get (k : cell) (CellCandidates l : cellCandidates) : digits =
        Smap.get Cell.comparer k l

    let make (l : (cell * digits) list) : cellCandidates =
        CellCandidates l
end

type solution = 
    { given : given;
      current : current;
      steps : action list }

module Solution = struct
    let givenToCurrent (cells : cells) (given : given) (alphabet : digits) : current =
        let makeCellContents (cell : cell) : cellContents =
            let dop = Given.get cell given in
            match dop with
            | Some digit -> BigNumber digit
            | None -> PencilMarks alphabet
            in

        cells
        |> Cells.to_list
        |> List.map (fun a -> (a, makeCellContents a))
        |> Current.make

    let currentCellCandidates (cells : cells) (current : current) : cellCandidates =
        let getCandidateEntries (cell : cell) : digits =
            let cellContents = Current.get cell current in
            match cellContents with
            | BigNumber _ -> Digits.empty
            | PencilMarks s -> s
            in

        cells
        |> Cells.to_list
        |> List.map (fun a -> (a, getCandidateEntries a))
        |> CellCandidates.make
end
