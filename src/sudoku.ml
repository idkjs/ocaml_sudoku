open Sset
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
type digits =
    {
        data : SSet<digit>
    }

F#*)

module Digits = struct
    let contains (d : digit) (s : digits) : bool = SSet.contains<digit> d s.data

    let count (s : digits) : int = SSet.count s.data

    let difference (s : digits) (s' : digits) : digits = { data = SSet.difference s.data s'.data }

    let empty : digits = { data = SSet.empty<digit> }

    let filter (predicate : digit -> bool) (s : digits) : digits = { data = SSet.filter predicate s.data }

    let intersect (s : digits) (s' : digits) = { data = SSet.intersect s.data s'.data }

    let isSubset (s : digits) (s' : digits) = SSet.isSubset s.data s'.data

    let nth (s : digits) (i : int) : digit = List.nth s.data.data i

    let ofList (as' : digit list) : digits = { data = SSet.ofList<digit> as' }

    let ofSet (s : SSet<digit>) : digits = { data = s }

    let remove (d : digit) (s : digits) : digits = { data = SSet.remove d s.data }

    let singleton (d : digit) : digits = { data = SSet.ofList [ d ] }

    let toList (s : digits) : digit list = SSet.toList s.data

    let union (s : digits) (s' : digits) : digits = { data = SSet.union s.data s'.data }

    let unionManyList (ss : digits list) : digits =
        let tss = List.map (fun s -> s.data) ss in
        { data = SSet.unionMany tss }

    let unionMany (ss : SSet<digits>) : digits =
        let tss =
            ss
            |> SSet.toList
            |> List.map (fun s -> s.data)
            in
        { data = SSet.unionMany tss }

    let tostring (digits : digits) : string =
        digits
        |> toList
        |> List.map digit_tostring
        |> String.concat ","
end

type cells = 
    {
        data : SSet<cell>
    }

module Cells = struct

    let choose (map : cell -> 'U option) (s : cells) : SSet<'U> = { data = List.choose map s.data.data }

    let contains (d : cell) (s : cells) : bool = SSet.contains<cell> d s.data

    let count (s : cells) : int = SSet.count<cell> s.data

    let difference (s : cells) (s' : cells) : cells = { data = SSet.difference s.data s'.data }

    let filter (predicate : cell -> bool) (s : cells) : cells = { data = SSet.filter predicate s.data }

    let map (map : cell -> 'U) (s : cells) : SSet<'U> = { data = List.map map s.data.data }

    let ofList (as' : cell list) : cells = { data = SSet.ofList<cell> as' }

    let ofSet (s : SSet<cell>) : cells = { data = s }

    let remove (d : cell) (s : cells) : cells = { data = SSet.remove<cell> d s.data }

    let singleton (d : cell) : cells = { data = SSet.ofList<cell> [ d ] }

    let toList (s : cells) : cell list = SSet.toList<cell> s.data

    let union (s : cells) (s' : cells) : cells = { data = SSet.union s.data s'.data }

    let unionManyList (ss : cells list) : cells =
        let tss = List.map (fun s -> s.data) ss in
        { data = SSet.unionMany<cell> tss }

    let unionMany (ss : SSet<cells>) : cells =
        let tss =
            ss
            |> SSet.toList
            |> List.map (fun s -> s.data)
            in
        { data = SSet.unionMany<cell> tss }
end

type columns = 
    {
        data : SSet<column>
    }

module Columns = struct

    let count (s : columns) : int = SSet.count<column> s.data

    let ofSet (s : SSet<column>) : columns = { data = s }

    let map (map : column -> 'U) (s : columns) : SSet<'U> = { data = List.map map s.data.data }

    let union (s : columns) (s' : columns) : columns = { data = SSet.union s.data s'.data }
end

type rows = 
    {
        data : SSet<row>
    }

module Rows = struct

    let count (s : rows) : int = SSet.count<row> s.data

    let ofSet (s : SSet<row>) : rows = { data = s }

    let map (map : row -> 'U) (s : rows) : SSet<'U> = { data = List.map map s.data.data }

    let union (s : rows) (s' : rows) : rows = { data = SSet.union s.data s'.data }
end

type houses = 
    {
        data : SSet<house>
    }

module Houses = struct

    let empty : houses = { data = SSet.empty<house> }

    let map (map : house -> 'U) (s : houses) : SSet<'U> = { data = List.map map s.data.data }

    let ofList (as' : house list) : houses = { data = SSet.ofList<house> as' }

    let ofSet (s : SSet<house>) : houses = { data = s }

    let singleton (d : house) : houses = { data = SSet.ofList [ d ] }

    let toList (houses : houses) : house list = SSet.toList<house> houses.data

    let tostring (houses : houses) : string =
        houses
        |> toList
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
 SSet the cell to have a Digit *)
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

type candidateReductions =
    { data : SSet<candidateReduction> }

module CandidateReductions = struct
    let count (s : candidateReductions) : int = SSet.count<candidateReduction> s.data

    let empty : candidateReductions = { data = SSet.empty<candidateReduction> }

    let filter (predicate : candidateReduction -> bool) (s : candidateReductions) : candidateReductions = { data = SSet.filter predicate s.data }

    let firstOpt (set : candidateReductions) = 
        if count set > 0 then set.data.data.Head |> Some
        else None

    let map (map : candidateReduction -> 'U) (s : candidateReductions) : SSet<'U> = { data = List.map map s.data.data }

    let ofSet (s : SSet<candidateReduction>) : candidateReductions = { data = s }

    let ofList(as' : candidateReduction list) : candidateReductions = { data = SSet.ofList<candidateReduction> as' }

    let singleton (d : candidateReduction) : candidateReductions = { data = SSet.ofList<candidateReduction> [ d ] }

    let toList (s : candidateReductions) : candidateReduction list = SSet.toList<candidateReduction> s.data

    let tostring (s : candidateReductions) : string =
        s
        |> toList
        |> List.map candidateReduction_tostring
        |> String.concat ","
end

(* Working towards a solution we take one of the following actions:
 SSet the cell to have a Digit
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

let givenToCurrent (cells : cell list) (given : given) (alphabet : digits) : current =
    let makeCellContents (cell : cell) : cellContents =
        let dop = SMap.get given cell in
        match dop with
        | Some digit -> BigNumber digit
        | None -> PencilMarks alphabet
        in

    SMap.ofLookup cells makeCellContents

(* for a cell, return a set of candidates *)
type cellCandidates = SMap<cell, digits>

let currentCellCandidates (cells : cell list) (current : current) : cellCandidates =
    let getCandidateEntries (cell : cell) : digits =
        let cellContents = SMap.get current cell in
        match cellContents with
        | BigNumber _ -> Digits.empty
        | PencilMarks s -> s
        in

    SMap.ofLookup<cell, digits> cells getCandidateEntries
