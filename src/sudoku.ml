open Smap

(* A sudoku is a square grid of size... *)
type size = int

(* containing columns... *)
type column = 
    | CColumn of int

let column_tostring (CColumn c : column) : string =
    Printf.sprintf "c%d" c

(* ... by rows *)
type row = 
    | RRow of int

let row_tostring (RRow r : row) : string =
    Printf.sprintf "r%d" r

(* Each cell is identified by (col, row) *)
type cell = 
    { col : column;
      row : row }

let cell_tostring ({col = CColumn c; row = RRow r} : cell) : string =
    Printf.sprintf "r%dc%d" r c

(* The grid is divided into boxes,
 these do not have to be square, but they are
 all the same size and cover the grid
 A column of vertical boxes is a stack *)
type stack = 
    | SStack of int

let stack_tostring (SStack s : stack) : string =
    Printf.sprintf "stk%d" s

type boxWidth = int

(* A row of horizontal boxes is a band *)
type band = 
    | BBand of int

let band_tostring (BBand b : band) : string =
    Printf.sprintf "bnd%d" b

type boxHeight = int

(* A box is the intersection of a stack and a band *)
type box = 
    { stack : stack;
      band : band }

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

let house_tostring (house : house) : string =
    match house with
    | HColumn column -> column_tostring column
    | HRow row -> row_tostring row
    | HBox box -> box_tostring box

(* Each cell in the grid contains a Digit, usually numbers 1..9 *)
type digit = 
    | Digit of char

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
    let contains (d : digit) (CDigits s : digits) : bool = Sset.contains<digit> d s

    let count (CDigits s : digits) : int = List.length s

    let difference (CDigits s : digits) (CDigits s' : digits) : digits = Sset.difference s s' |> CDigits

    let empty : digits = [] |> CDigits

    let filter (predicate : digit -> bool) (CDigits s : digits) : digits = List.filter predicate s |> CDigits

    let first (CDigits d : digits) : digit = 
        match d with
        | h :: _ -> h
        | [] -> failwith "Not empty"

    let intersect (CDigits s : digits) (CDigits s' : digits) : digits = Sset.intersect s s' |> CDigits

    let isSubset (CDigits s : digits) (CDigits s' : digits) : bool = Sset.isSubset s s'

    let nth (CDigits s : digits) (i : int) : digit = List.nth s i

    let ofList (as' : digit list) : digits = Sset.ofList<digit> as' |> CDigits

    let remove (d : digit) (CDigits s : digits) : digits = Sset.remove d s |> CDigits

    let singleton (d : digit) : digits = [ d ] |> CDigits

    let toList (CDigits s : digits) : digit list = s

    let union (CDigits s : digits) (CDigits s' : digits) : digits = Sset.union s s' |> CDigits

    let unionManyList (ss : digits list) : digits =
        let tss = List.map toList ss in
        Sset.unionMany tss |> CDigits

    let tostring (CDigits digits : digits) : string =
        digits
        |> List.map digit_tostring
        |> String.concat ","
end

type cells = CCells of cell list

module Cells = struct

    let choose (map : cell -> 'b option) (CCells s : cells) : 'b list = List.choose map s

    let contains (d : cell) (CCells s : cells) : bool = Sset.contains<cell> d s

    let count (CCells s : cells) : int = List.length s

    let difference (CCells s : cells) (CCells s' : cells) : cells = Sset.difference s s' |> CCells

    let filter (predicate : cell -> bool) (CCells s : cells) : cells = List.filter predicate s |> CCells

    let map (map : cell -> 'b) (CCells s : cells) : 'b list = List.map map s

    let ofList (as' : cell list) : cells = Sset.ofList<cell> as' |> CCells

    let remove (d : cell) (CCells s : cells) : cells = Sset.remove<cell> d s |> CCells

    let singleton (d : cell) : cells = [ d ] |> CCells

    let toList (CCells s : cells) : cell list = s

    let union (CCells s : cells) (CCells s' : cells) : cells = Sset.union s s' |> CCells

    let unionManyList (ss : cells list) : cells =
        let tss = List.map toList ss in
        Sset.unionMany<cell> tss |> CCells
end

type columns = CColumns of column list

module Columns = struct

    let count (CColumns s : columns) : int = List.length s

    let drop (n : int) (CColumns columns : columns) : columns = Sset.drop n columns |> CColumns

    let ofList (s : column list) : columns = Sset.ofList<column> s |> CColumns

    let map (map : column -> 'b) (CColumns s : columns) : 'b list = List.map map s

    let mapi (map : int -> column -> 'b) (CColumns s : columns) : 'b list = List.mapi map s

    let union (CColumns s : columns) (CColumns s' : columns) : columns = Sset.union s s' |> CColumns
end

type rows = CRows of row list

module Rows = struct

    let count (CRows s : rows) : int = List.length s

    let drop (n : int) (CRows rows : rows) : rows = Sset.drop n rows |> CRows

    let ofList (s : row list) : rows = Sset.ofList s |> CRows

    let map (map : row -> 'b) (CRows s : rows) : 'b list = List.map map s

    let mapi (map : int -> row -> 'b) (CRows s : rows) : 'b list = List.mapi map s

    let union (CRows s : rows) (CRows s' : rows) : rows = Sset.union s s' |> CRows
end

type houses = CHouses of house list

module Houses = struct

    let drop (n : int) (CHouses houses : houses) : houses = Sset.drop n houses |> CHouses

    let empty : houses = [] |> CHouses

    let map (map : house -> 'b) (CHouses s : houses) : 'b list = List.map map s

    let mapi (map : int -> house -> 'b) (CHouses s : houses) : 'b list = List.mapi map s

    let ofList (as' : house list) : houses = Sset.ofList<house> as' |> CHouses

    let singleton (d : house) : houses = [ d ] |> CHouses

    let toList (CHouses houses : houses) : house list = houses

    let tostring (CHouses houses : houses) : string =
        houses
        |> List.map house_tostring
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

type candidateReductions = CCandidateReductions of candidateReduction list

module CandidateReductions = struct
    let count (CCandidateReductions s : candidateReductions) : int = List.length s

    let empty : candidateReductions = [] |> CCandidateReductions

    let filter (predicate : candidateReduction -> bool) (CCandidateReductions s : candidateReductions) : candidateReductions = List.filter predicate s |> CCandidateReductions

    let first (CCandidateReductions set : candidateReductions) : candidateReduction = 
        match set with
        | h :: _ -> h
        | [] -> failwith "Not empty"

    let firstOpt (CCandidateReductions set : candidateReductions) : candidateReduction option = 
        match set with
        | h :: _ -> h |> Some
        | [] -> None

    let map (map : candidateReduction -> 'b) (CCandidateReductions s : candidateReductions) : 'b list = List.map map s

    let ofList(as' : candidateReduction list) : candidateReductions = Sset.ofList<candidateReduction> as' |> CCandidateReductions

    let singleton (d : candidateReduction) : candidateReductions = [ d ] |> CCandidateReductions

    let toList (CCandidateReductions s : candidateReductions) : candidateReduction list = s

    let tostring (CCandidateReductions s : candidateReductions) : string =
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
