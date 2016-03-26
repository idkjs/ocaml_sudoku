open Smap

(* A sudoku is a square grid of size... *)
type size = int

(* containing columns... *)
type column = 
    | CColumn of int

let column_comparer (CColumn c1 : column) (CColumn c2 : column) : int =
    if c1 < c2 then -1
    else if c1 = c2 then 0
    else 1

let column_tostring (CColumn c : column) : string =
    Printf.sprintf "c%d" c

type columns = CColumns of column list

module Columns = struct

    let count (CColumns s : columns) : int = List.length s

    let drop (n : int) (CColumns columns : columns) : columns = Sset.drop n columns |> CColumns

    let ofList (s : column list) : columns = Sset.ofList<column> column_comparer s |> CColumns

    let map (map : column -> 'b) (CColumns s : columns) : 'b list = List.map map s

    let mapi (map : int -> column -> 'b) (CColumns s : columns) : 'b list = List.mapi map s

    let toList (CColumns s : columns) : column list = s

    let list_to_string (s : column list) : string =
        s
        |> List.map column_tostring
        |> String.concat ","

    let tostring (CColumns s : columns) : string = list_to_string s

    let union (CColumns s : columns) (CColumns s' : columns) : columns = Sset.union column_comparer s s' |> CColumns
end

(* ... by rows *)
type row = 
    | RRow of int

let row_comparer (RRow r1 : row) (RRow r2 : row) : int =
    if r1 < r2 then -1
    else if r1 = r2 then 0
    else 1

let row_tostring (RRow r : row) : string =
    Printf.sprintf "r%d" r

type rows = CRows of row list

module Rows = struct

    let count (CRows s : rows) : int = List.length s

    let drop (n : int) (CRows rows : rows) : rows = Sset.drop n rows |> CRows

    let ofList (s : row list) : rows = Sset.ofList row_comparer s |> CRows

    let map (map : row -> 'b) (CRows s : rows) : 'b list = List.map map s

    let mapi (map : int -> row -> 'b) (CRows s : rows) : 'b list = List.mapi map s

    let toList (CRows s : rows) : row list = s

    let list_to_string (s : row list) : string =
        s
        |> List.map row_tostring
        |> String.concat ","

    let tostring (CRows s : rows) : string = list_to_string s

    let union (CRows s : rows) (CRows s' : rows) : rows = Sset.union row_comparer s s' |> CRows
end

(* Each cell is identified by (col, row) *)
type cell = 
    { col : column;
      row : row }

let cell_comparer ({ col = CColumn c1; row = RRow r1} : cell) ({ col = CColumn c2; row = RRow r2} : cell) : int =
    if r1 < r2 then -1
    else if r1 = r2 then
        if c1 < c2 then -1
        else if c1 = c2 then 0
        else 1
    else 1

let cell_tostring ({col = CColumn c; row = RRow r} : cell) : string =
    Printf.sprintf "r%dc%d" r c

type cells = CCells of cell list

module Cells = struct

    let choose (map : cell -> 'b option) (CCells s : cells) : 'b list = List.choose map s

    let contains (d : cell) (CCells s : cells) : bool = Sset.contains<cell> cell_comparer d s

    let count (CCells s : cells) : int = List.length s

    let difference (CCells s : cells) (CCells s' : cells) : cells = Sset.subtract cell_comparer s s' |> CCells

    let filter (predicate : cell -> bool) (CCells s : cells) : cells = List.filter predicate s |> CCells

    let map (map : cell -> 'b) (CCells s : cells) : 'b list = List.map map s

    let ofList (as' : cell list) : cells = Sset.ofList<cell> cell_comparer as' |> CCells

    let remove (d : cell) (CCells s : cells) : cells = Sset.remove<cell> cell_comparer d s |> CCells

    let singleton (d : cell) : cells = [ d ] |> CCells

    let toList (CCells s : cells) : cell list = s

    let list_to_string (s : cell list) : string =
        s
        |> List.map cell_tostring
        |> String.concat ","

    let tostring (CCells s : cells) : string = list_to_string s

    let union (CCells s : cells) (CCells s' : cells) : cells = Sset.union cell_comparer s s' |> CCells

    let unionManyList (ss : cells list) : cells =
        let tss = List.map toList ss in
        Sset.unions cell_comparer tss |> CCells
end


(* The grid is divided into boxes,
 these do not have to be square, but they are
 all the same size and cover the grid
 A column of vertical boxes is a stack *)
type stack = 
    | SStack of int

let stack_comparer (SStack s1 : stack) (SStack s2 : stack) : int =
    if s1 < s2 then -1
    else if s1 = s2 then 0
    else 1

let stack_tostring (SStack s : stack) : string =
    Printf.sprintf "stk%d" s

type boxWidth = int

(* A row of horizontal boxes is a band *)
type band = 
    | BBand of int

let band_comparer (BBand b1 : band) (BBand b2 : band) : int =
    if b1 < b2 then -1
    else if b1 = b2 then 0
    else 1

let band_tostring (BBand b : band) : string =
    Printf.sprintf "bnd%d" b

type boxHeight = int

(* A box is the intersection of a stack and a band *)
type box = 
    { stack : stack;
      band : band }

let box_comparer ({ stack = SStack s1; band = BBand b1} : box) ({ stack = SStack s2; band = BBand b2} : box) : int =
    if b1 < b2 then -1
    else if b1 = b2 then
        if s1 < s2 then -1
        else if s1 = s2 then 0
        else 1
    else 1

let box_tostring ({stack = SStack s; band = BBand b} : box) : string =
    Printf.sprintf "stk%dbnd%d" s b

(* The columns and rows are collectively called lines *)
type line = 
    | LColumn of column
    | LRow of row

(* The columns, rows and boxes are collectively called houses *)
type house = 
    | HColumn of column
    | HRow of row
    | HBox of box

let house_comparer (h1 : house) (h2 : house) : int =
    match h1, h2 with
    | HColumn c1, HColumn c2 -> column_comparer c1 c2
    | HColumn _, HRow _ -> -1
    | HColumn _, HBox _ -> -1
    | HRow _, HColumn _ -> 1
    | HRow r1, HRow r2 -> row_comparer r1 r2
    | HRow _, HBox _ -> -1
    | HBox _, HColumn _ -> 1
    | HBox _, HRow _ -> 1
    | HBox b1, HBox b2 -> box_comparer b1 b2

let house_tostring (house : house) : string =
    match house with
    | HColumn column -> column_tostring column
    | HRow row -> row_tostring row
    | HBox box -> box_tostring box

type houses = CHouses of house list

module Houses = struct

    let drop (n : int) (CHouses houses : houses) : houses = Sset.drop n houses |> CHouses

    let empty : houses = [] |> CHouses

    let map (map : house -> 'b) (CHouses s : houses) : 'b list = List.map map s

    let mapi (map : int -> house -> 'b) (CHouses s : houses) : 'b list = List.mapi map s

    let ofList (as' : house list) : houses = Sset.ofList<house> house_comparer as' |> CHouses

    let singleton (d : house) : houses = [ d ] |> CHouses

    let toList (CHouses houses : houses) : house list = houses

    let tostring (CHouses houses : houses) : string =
        houses
        |> List.map house_tostring
        |> String.concat ","
end

(* Each cell in the grid contains a Digit, usually numbers 1..9 *)
type digit = 
    | Digit of char

let digit_comparer (Digit d1 : digit) (Digit d2 : digit) : int =
    if d1 < d2 then -1
    else if d1 = d2 then 0
    else 1

let digit_tostring (Digit s : digit) : string =
    (string) s

(*IF-OCAML*)
module Digit =
    struct
        type t = digit
        let compare = Pervasives.compare
    end
module ModDigitSet = Set.Make(Digit)
(*ENDIF-OCAML*)
(*F#
type digits = CDigits of digit list

F#*)

module Digits = struct
    let contains (d : digit) (CDigits s : digits) : bool = Sset.contains<digit> digit_comparer d s

    let count (CDigits s : digits) : int = List.length s

    let difference (CDigits s : digits) (CDigits s' : digits) : digits = Sset.subtract digit_comparer s s' |> CDigits

    let empty : digits = [] |> CDigits

    let filter (predicate : digit -> bool) (CDigits s : digits) : digits = List.filter predicate s |> CDigits

    let first (CDigits d : digits) : digit = 
        match d with
        | h :: _ -> h
        | [] -> failwith "Not empty"

    let intersect (CDigits s : digits) (CDigits s' : digits) : digits = Sset.intersect digit_comparer s s' |> CDigits

    let isSubset (CDigits s : digits) (CDigits s' : digits) : bool = Sset.isSubset s s'

    let nth (CDigits s : digits) (i : int) : digit = List.item i s

    let ofList (as' : digit list) : digits = Sset.ofList<digit> digit_comparer as' |> CDigits

    let remove (d : digit) (CDigits s : digits) : digits = Sset.remove digit_comparer d s |> CDigits

    let singleton (d : digit) : digits = [ d ] |> CDigits

    let toList (CDigits s : digits) : digit list = s

    let union (CDigits s : digits) (CDigits s' : digits) : digits = Sset.union digit_comparer s s' |> CDigits

    let unionManyList (ss : digits list) : digits =
        let tss = List.map toList ss in
        Sset.unions digit_comparer tss |> CDigits

    let tostring (CDigits digits : digits) : string =
        digits
        |> List.map digit_tostring
        |> String.concat ","
end




(* A sudoku is defined by the overall grid size (it is always square)
 which is the same as the Digits in the alphabet
 and also by the width and height of the boxes *)
type puzzleShape = 
    { size : size;
      boxWidth : boxWidth;
      boxHeight : boxHeight;
      alphabet : digits }

let makeDigit i = (char) i + '0' |> Digit

(*let rec range i j = if i > j then [] else i :: (range (i+1) j)*)

let defaultPuzzleSpec : puzzleShape = 
    { size = 9;
      boxWidth = 3;
      boxHeight = 3;
      alphabet =
        [1..9]
        |> List.map makeDigit
        |> Digits.ofList
        }

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

let value_tostring ({ cell = cell; digit = digit} : value) : string =
    Printf.sprintf "%s=%s" (cell_tostring cell) (digit_tostring digit)

(* A candidate is a digit in a cell, which is still a pencilmark *)
type candidate = 
    { cell : cell;
      digit : digit }

let candidate_tostring ({ cell = cell; digit = digit} : candidate) : string =
    Printf.sprintf "(%s)%s" (cell_tostring cell) (digit_tostring digit)

type candidateReduction = 
    { cell : cell;
      candidates : digits }

let candidateReduction_tostring ({ cell = cell; candidates = digits} : candidateReduction) : string =
    Printf.sprintf "Cell %s, Candidates %s" (cell_tostring cell) (Digits.tostring digits)

module CandidateReductions = struct
    let tostring (s : candidateReduction list) : string =
        s
        |> List.map candidateReduction_tostring
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

let action_tostring (action : action) : string =
    match action with
    | Load sudoku -> Printf.sprintf "Load:%s" sudoku
    | LoadEliminate  -> "Load"
    | Placement a -> Printf.sprintf "%s=%s" (cell_tostring a.cell) (digit_tostring a.digit)
    | Eliminate candidate -> Printf.sprintf "%s<>%s" (cell_tostring candidate.cell) (digit_tostring candidate.digit)

type given = SMap<cell, digit option>

type current = SMap<cell, cellContents>

[<NoComparisonAttribute>]
type solution = 
    { given : given;
      current : current;
      steps : action list }

let givenToCurrent (cells : cells) (given : given) (alphabet : digits) : current =
    let makeCellContents (cell : cell) : cellContents =
        let dop = SMap.get given cell in
        match dop with
        | Some digit -> BigNumber digit
        | None -> PencilMarks alphabet
        in

    SMap.ofLookup (Cells.toList cells) makeCellContents

(* for a cell, return a set of candidates *)
type cellCandidates = SMap<cell, digits>

let currentCellCandidates (cells : cells) (current : current) : cellCandidates =
    let getCandidateEntries (cell : cell) : digits =
        let cellContents = SMap.get current cell in
        match cellContents with
        | BigNumber _ -> Digits.empty
        | PencilMarks s -> s
        in

    SMap.ofLookup<cell, digits> (Cells.toList cells) getCandidateEntries
