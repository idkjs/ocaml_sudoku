(*F#
module core.sudoku
F#*)
open System

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

(* A sudoku is defined by the overall grid size (it is always square)
 which is the same as the Digits in the alphabet
 and also by the width and height of the boxes *)
type puzzleShape = 
    { size : size;
      boxWidth : boxWidth;
      boxHeight : boxHeight;
      alphabet : digit list }

(*IF-OCAML*)
module Digit =
    struct
        type t = digit

        let compare d0 d1 =
        Pervasives.compare d0 d1
    end

module ModDigitSet = Set.Make(Digit)
(*ENDIF-OCAML*)
(*F#
type DigitSet = Set<digit>
F#*)

(* Whilst working to a solution each cell in the grid
 that doesn't have a Digit is filled with candidates
 Candidates are possible Digits *)
type cellContents = 
    | BigNumber of digit
    | PencilMarks of DigitSet

(* Working towards a solution we take one of the following actions:
 Set the cell to have a Digit *)
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
 Set the cell to have a Digit
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

type given = Map<cell, digit option>

type current = Map<cell, cellContents>

type solution = 
    { given : given;
      current : current;
      steps : action list }

(* From http://www.fssnip.net/ji *)
type either<'a, 'b> = 
    | Left of 'a
    | Right of 'b

type lookup<'a, 'b> = 
    abstract member Get: 'a -> 'b

type mapLookup<'a, 'b when 'a : comparison>(data : Map<'a, 'b>) =
    interface lookup<'a, 'b> with
        member this.Get (a : 'a) =
            data.Item a

(* for a cell, return a set of candidates *)
type cellCandidates = lookup<cell, Set<digit>>

let currentCellCandidates (current : current) : cellCandidates =
    let getCandidateEntries (_ : cell) (annotatedDigit : cellContents) : Set<digit> =
        match annotatedDigit with
        | BigNumber _ -> Set.empty
        | PencilMarks s -> s

    let candidateLookup =
        current
        |> Map.map getCandidateEntries

    mapLookup<cell, Set<digit>> candidateLookup :> cellCandidates
