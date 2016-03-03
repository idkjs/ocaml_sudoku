(*F#
module core.sudoku

open System
open sset
F#*)

(* A sudoku is a square grid of size... *)
type size = int

(* containing columns... *)
type column = 
    | CColumn of int
(*F#
    override this.ToString() =
        match this with CColumn c -> String.Format("c{0}", c)
F#*)

(* ... by rows *)
type row = 
    | RRow of int
(*F#
    override this.ToString() =
        match this with RRow r -> String.Format("r{0}", r)
F#*)

(* Each cell is identified by (col, row) *)
type cell = 
    { col : column;
      row : row }
(*F#
    override this.ToString() =
        String.Format("{0}{1}", this.row, this.col)
F#*)

(* The grid is divided into boxes,
 these do not have to be square, but they are
 all the same size and cover the grid
 A column of vertical boxes is a stack *)
type stack = 
    | SStack of int
(*F#
    override this.ToString() =
        match this with SStack s -> String.Format("stk{0}", s)
F#*)

type boxWidth = int

(* A row of horizontal boxes is a band *)
type band = 
    | BBand of int
(*F#
    override this.ToString() =
        match this with BBand b -> String.Format("bnd{0}", b)
F#*)

type boxHeight = int

(* A box is the intersection of a stack and a band *)
type box = 
    { stack : stack;
      band : band }
(*F#
    override this.ToString() =
        String.Format("{0}{1}", this.stack, this.band)
F#*)

(* The columns and rows are collectively called lines *)
type line = 
    | LColumn of column
    | LRow of row

(* The columns, rows and boxes are collectively called houses *)
type house = 
    | HColumn of column
    | HRow of row
    | HBox of box
(*F#
    override this.ToString() = 
        match this with
        | HColumn c -> c.ToString()
        | HRow r -> r.ToString()
        | HBox b -> b.ToString()
F#*)

(* Each cell in the grid contains a Digit, usually numbers 1..9 *)
type digit = 
    | Digit of char
(*F#
    override this.ToString() =
        match this with Digit s -> (string) s
F#*)

(*IF-OCAML*)
module Digit =
    struct
        type t = digit
        let compare = Pervasives.compare
    end
module ModDigitSet = SSet.Make(Digit)
(*ENDIF-OCAML*)
(*F#
type digits =
    {
        data : SSet<digit>
    }

module Digits =
    let contains (d : digit) (s : digits) : bool = SSet.contains<digit> d s.data

    let count (s : digits) : int = SSet.count s.data

    let difference (s : digits) (s' : digits) : digits = { data = SSet.difference s.data s'.data }

    let empty : digits = { data = SSet.empty<digit> }

    let filter (predicate : digit -> bool) (s : digits) : digits = { data = SSet.filter predicate s.data }

    let intersect (s : digits) (s' : digits) = { data = SSet.intersect s.data s'.data }

    let isSubset (s : digits) (s' : digits) = SSet.isSubset s.data s'.data

    let ofArray (as' : digit array) : digits = { data = SSet.ofArray<digit> as' }

    let ofSet (s : SSet<digit>) : digits = { data = s }

    let remove (d : digit) (s : digits) : digits = { data = SSet.remove d s.data }

    let singleton (d : digit) : digits = { data = SSet.ofArray [| d |] }

    let toArray (s : digits) : digit array = SSet.toArray s.data

    let union (s : digits) (s' : digits) : digits = { data = SSet.union s.data s'.data }

    let unionMany (ss : seq<digits>) : digits =
        let tss =
            ss
            |> Seq.map (fun s -> s.data)
        { data = SSet.unionMany tss }

type cells = 
    {
        data : SSet<cell>
    }

module Cells =

    let choose (map : cell -> 'U option) (s : cells) : SSet<'U> = { data = List.choose map s.data.data }

    let contains (d : cell) (s : cells) : bool = SSet.contains<cell> d s.data

    let count (s : cells) : int = SSet.count<cell> s.data

    let difference (s : cells) (s' : cells) : cells = { data = SSet.difference s.data s'.data }

    let filter (predicate : cell -> bool) (s : cells) : cells = { data = SSet.filter predicate s.data }

    let map (map : cell -> 'U) (s : cells) : SSet<'U> = { data = List.map map s.data.data }

    let ofArray (as' : cell array) : cells = { data = SSet.ofArray<cell> as' }

    let ofSet (s : SSet<cell>) : cells = { data = s }

    let remove (d : cell) (s : cells) : cells = { data = SSet.remove<cell> d s.data }

    let singleton (d : cell) : cells = { data = SSet.ofArray<cell> [| d |] }

    let toArray (s : cells) : cell array = SSet.toArray<cell> s.data

    let union (s : cells) (s' : cells) : cells = { data = SSet.union s.data s'.data }

    let unionMany (ss : seq<cells>) : cells =
        let tss =
            ss
            |> Seq.map (fun s -> s.data)
        { data = SSet.unionMany<cell> tss }

type columns = 
    {
        data : SSet<column>
    }

module Columns =

    let count (s : columns) : int = SSet.count<column> s.data

    let ofSet (s : SSet<column>) : columns = { data = s }

    let map (map : column -> 'U) (s : columns) : SSet<'U> = { data = List.map map s.data.data }

    let union (s : columns) (s' : columns) : columns = { data = SSet.union s.data s'.data }

type rows = 
    {
        data : SSet<row>
    }

module Rows =

    let count (s : rows) : int = SSet.count<row> s.data

    let ofSet (s : SSet<row>) : rows = { data = s }

    let map (map : row -> 'U) (s : rows) : SSet<'U> = { data = List.map map s.data.data }

    let union (s : rows) (s' : rows) : rows = { data = SSet.union s.data s'.data }

type houses = 
    {
        data : SSet<house>
    }

module Houses =

    let empty : houses = { data = SSet.empty<house> }

    let map (map : house -> 'U) (s : houses) : SSet<'U> = { data = List.map map s.data.data }

    let ofArray (as' : house array) : houses = { data = SSet.ofArray<house> as' }

    let ofSet (s : SSet<house>) : houses = { data = s }

    let singleton (d : house) : houses = { data = SSet.ofArray [| d |] }

F#*)

(* A sudoku is defined by the overall grid size (it is always square)
 which is the same as the Digits in the alphabet
 and also by the width and height of the boxes *)
type puzzleShape = 
    { size : size;
      boxWidth : boxWidth;
      boxHeight : boxHeight;
      alphabet : digit array }


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
(*F#
    override this.ToString() =
        String.Format("{0}={1}", this.cell, this.digit)
F#*)

(* A candidate is a digit in a cell, which is still a pencilmark *)
type candidate = 
    { cell : cell;
      digit : digit }
(*F#
    override this.ToString() =
        String.Format("({0}){1}", this.digit, this.cell)
F#*)

(* Working towards a solution we take one of the following actions:
 SSet the cell to have a Digit
 or remove a candidate *)
type action = 
    | Placement of value
    | Eliminate of candidate
(*F#
    override this.ToString() =
        match this with
        | Placement a -> String.Format("{0}={1}", a.cell, a.digit)
        | Eliminate candidate -> String.Format("{0}<>{1}", candidate.cell, candidate.digit)
F#*)

type lookup<'a, 'b> = 
    abstract member Get: 'a -> 'b

type given = lookup<cell, digit option>

type current = lookup<cell, cellContents>

[<NoComparisonAttribute>]
type solution = 
    { given : given;
      current : current;
      steps : action list }

(* From http://www.fssnip.net/ji *)
type either<'a, 'b> = 
    | Left of 'a
    | Right of 'b


type mapLookup<'a, 'b when 'a : comparison>(data : Map<'a, 'b>) =
    interface lookup<'a, 'b> with
        member this.Get (a : 'a) =
            data.Item a

let makeMapLookup<'a, 'b when 'a : comparison> (as' : 'a array) (fn : 'a -> 'b) : mapLookup<'a, 'b> =
    as'
    |> Array.map (fun a -> (a, fn a))
    |> Map.ofSeq
    |> mapLookup<'a, 'b>

(* for a cell, return a set of candidates *)
type cellCandidates = lookup<cell, digits>

let currentCellCandidates (cells : cell array) (current : current) : cellCandidates =
    let getCandidateEntries (cell : cell) : digits =
        let cellContents = current.Get cell
        match cellContents with
        | BigNumber _ -> Digits.empty
        | PencilMarks s -> s

    makeMapLookup<cell, digits> cells getCandidateEntries :> cellCandidates

type candidateReduction = 
    { cell : cell
      candidates : digits }
    override this.ToString() = 
        String.Format("Cell {0}, Candidates {1}", this.cell, String.Join(",", this.candidates))

type candidateReductions = 
    {
        data : SSet<candidateReduction>
    }

module CandidateReductions =
    let count (s : candidateReductions) : int = SSet.count<candidateReduction> s.data

    let empty : candidateReductions = { data = SSet.empty<candidateReduction> }

    let filter (predicate : candidateReduction -> bool) (s : candidateReductions) : candidateReductions = { data = SSet.filter predicate s.data }

    let firstOpt (set : candidateReductions) = 
        if count set > 0 then set.data.data.Head |> Some
        else None

    let map (map : candidateReduction -> 'U) (s : candidateReductions) : SSet<'U> = { data = List.map map s.data.data }

    let ofSet (s : SSet<candidateReduction>) : candidateReductions = { data = s }

    let ofArray (as' : candidateReduction array) : candidateReductions = { data = SSet.ofArray<candidateReduction> as' }

    let singleton (d : candidateReduction) : candidateReductions = { data = SSet.ofArray<candidateReduction> [| d |] }

    let toArray (s : candidateReductions) : candidateReduction array = SSet.toArray<candidateReduction> s.data
