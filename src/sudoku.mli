type size = int
type column = | CColumn of int
module Column = begin
  val make : int -> column
  val comparer : column -> column -> int
  val to_string : column -> string
end
type columns = | CColumns of column list
module Columns = begin
  val count : columns -> int
  val drop : n:int -> columns -> columns
  val ofList : s:column list -> columns
  val map : map:(column -> 'b) -> columns -> 'b list
  val mapi : map:(int -> column -> 'b) -> columns -> 'b list
  val toList : columns -> column list
  val list_to_string : s:column list -> string
  val to_string : columns -> string
  val union : columns -> columns -> columns
end
type row = | RRow of int
module Row = begin
  val make : int -> row
  val comparer : row -> row -> int
  val to_string : row -> string
end
type rows = | CRows of row list
module Rows = begin
  val count : rows -> int
  val drop : n:int -> rows -> rows
  val ofList : s:row list -> rows
  val map : map:(row -> 'b) -> rows -> 'b list
  val mapi : map:(int -> row -> 'b) -> rows -> 'b list
  val toList : rows -> row list
  val list_to_string : s:row list -> string
  val to_string : rows -> string
  val union : rows -> rows -> rows
end
type cell =
  {col: column;
   row: row;}
module Cell = begin
  val make : column -> row -> cell
  val comparer : cell -> cell -> int
  val to_string : cell -> string
end
type cells = | CCells of cell list
module Cells = begin
  val choose : map:(cell -> 'b option) -> cells -> 'b list
  val contains : d:cell -> cells -> bool
  val count : cells -> int
  val difference : cells -> cells -> cells
  val filter : predicate:(cell -> bool) -> cells -> cells
  val map : map:(cell -> 'b) -> cells -> 'b list
  val ofList : as':cell list -> cells
  val remove : d:cell -> cells -> cells
  val singleton : d:cell -> cells
  val toList : cells -> cell list
  val list_to_string : s:cell list -> string
  val to_string : cells -> string
  val union : cells -> cells -> cells
  val unionManyList : ss:cells list -> cells
end
type stack = | SStack of int
module Stack = begin
  val make : int -> stack
  val comparer : stack -> stack -> int
  val to_string : stack -> string
end
module Stacks = begin
  val list_to_string : s:stack list -> string
end
type boxWidth = int
type band = | BBand of int
module Band = begin
  val make : int -> band
  val comparer : band -> band -> int
  val to_string : band -> string
end
module Bands = begin
  val list_to_string : s:band list -> string
end
type boxHeight = int
type box =
  {stack: stack;
   band: band;}
module Box = begin
  val make : stack -> band -> box
  val comparer : box -> box -> int
  val to_string : box -> string
end
module Boxes = begin
  val list_to_string : s:box list -> string
end
type line =
  | LColumn of column
  | LRow of row
type house =
  | HColumn of column
  | HRow of row
  | HBox of box
module House = begin
  val comparer : h1:house -> h2:house -> int
  val to_string : house:house -> string
end
type houses = | CHouses of house list
module Houses = begin
  val drop : n:int -> houses -> houses
  val empty : houses
  val map : map:(house -> 'b) -> houses -> 'b list
  val mapi : map:(int -> house -> 'b) -> houses -> 'b list
  val ofList : as':house list -> houses
  val singleton : d:house -> houses
  val toList : houses -> house list
  val list_to_string : s:house list -> string
  val to_string : houses -> string
end
type digit = | Digit of char
module Digit = begin
  val make : i:int -> digit
  val comparer : digit -> digit -> int
  val to_string : digit -> string
end
type digits = | CDigits of digit list
module Digits = begin
  val contains : d:digit -> digits -> bool
  val count : digits -> int
  val difference : digits -> digits -> digits
  val empty : digits
  val filter : predicate:(digit -> bool) -> digits -> digits
  val first : digits -> digit
  val intersect : digits -> digits -> digits
  val isSubset : digits -> digits -> bool
  val nth : digits -> i:int -> digit
  val ofList : as':digit list -> digits
  val remove : d:digit -> digits -> digits
  val singleton : d:digit -> digits
  val toList : digits -> digit list
  val union : digits -> digits -> digits
  val unionManyList : ss:digits list -> digits
  val list_to_string : s:digit list -> string
  val to_string : digits -> string
end
type puzzleShape =
  {size: size;
   boxWidth: boxWidth;
   boxHeight: boxHeight;
   alphabet: digits;}
module PuzzleShape = begin
    val default' : puzzleShape
end
type cellContents =
  | BigNumber of digit
  | PencilMarks of digits
type value =
  {cell: cell;
   digit: digit;}
module Value = begin
  val make : cell -> digit -> value
  val to_string : value -> string
end
type candidate =
  {cell: cell;
   digit: digit;}
module Candidate = begin
  val make : cell -> digit -> candidate
  val to_string : candidate -> string
end
type candidateReduction =
  {cell: cell;
   candidates: digits;}
module CandidateReduction = begin
  val make : cell -> digits -> candidateReduction
  val to_string : candidateReduction -> string
end
module CandidateReductions = begin
  val to_string : s:candidateReduction list -> string
end
[<NoComparisonAttribute ()>]
type action =
  | Load of string
  | LoadEliminate
  | Placement of value
  | Eliminate of candidate
module Action = begin
  val to_string : action:action -> string
end
type given = Given of (cell * digit option) list
module Given = begin
    val get : given:given -> key:cell -> digit option
end
type current = Current of (cell * cellContents) list
module Current = begin
    val get : current:current -> key:cell -> cellContents
end
type cellCandidates = CellCandidates of (cell * digits) list
module CellCandidates = begin
    val get : cellCandidates:cellCandidates -> key:cell -> digits
end
[<NoComparisonAttribute ()>]
type solution =
  {given: given;
   current: current;
   steps: action list;}
module Solution = begin
    val givenToCurrent : cells:cells -> given:given -> alphabet:digits -> current
    val currentCellCandidates : cells:cells -> current:current -> cellCandidates
end
