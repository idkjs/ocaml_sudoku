type size = int
type column = | CColumn of int
module Column : sig
  val make : int -> column
  val comparer : column -> column -> int
  val to_string : column -> string
end
type columns = | CColumns of column list
module Columns : sig
  val count : columns -> int
  val drop : int -> columns -> columns
  val ofList : column list -> columns
  val map : (column -> 'b) -> columns -> 'b list
  val mapi : (int -> column -> 'b) -> columns -> 'b list
  val toList : columns -> column list
  val list_to_string : column list -> string
  val to_string : columns -> string
  val union : columns -> columns -> columns
end
type row = | RRow of int
module Row : sig
  val make : int -> row
  val comparer : row -> row -> int
  val to_string : row -> string
end
type rows = | CRows of row list
module Rows : sig
  val count : rows -> int
  val drop : int -> rows -> rows
  val ofList : row list -> rows
  val map : (row -> 'b) -> rows -> 'b list
  val mapi : (int -> row -> 'b) -> rows -> 'b list
  val toList : rows -> row list
  val list_to_string : row list -> string
  val to_string : rows -> string
  val union : rows -> rows -> rows
end
type cell =
  {col: column;
   row: row;}
module Cell : sig
  val make : column -> row -> cell
  val comparer : cell -> cell -> int
  val to_string : cell -> string
end
type cells = | CCells of cell list
module Cells : sig
  val choose : (cell -> 'b option) -> cells -> 'b list
  val contains : cell -> cells -> bool
  val count : cells -> int
  val difference : cells -> cells -> cells
  val filter : (cell -> bool) -> cells -> cells
  val map : (cell -> 'b) -> cells -> 'b list
  val ofList : cell list -> cells
  val remove : cell -> cells -> cells
  val singleton : cell -> cells
  val toList : cells -> cell list
  val list_to_string : cell list -> string
  val to_string : cells -> string
  val union : cells -> cells -> cells
  val unionManyList : cells list -> cells
end
type stack = | SStack of int
module Stack : sig
  val make : int -> stack
  val comparer : stack -> stack -> int
  val to_string : stack -> string
end
module Stacks : sig
  val list_to_string : stack list -> string
end
type boxWidth = int
type band = | BBand of int
module Band : sig
  val make : int -> band
  val comparer : band -> band -> int
  val to_string : band -> string
end
module Bands : sig
  val list_to_string : band list -> string
end
type boxHeight = int
type box =
  {stack: stack;
   band: band;}
module Box : sig
  val make : stack -> band -> box
  val comparer : box -> box -> int
  val to_string : box -> string
end
module Boxes : sig
  val list_to_string : box list -> string
end
type line =
  | LColumn of column
  | LRow of row
type house =
  | HColumn of column
  | HRow of row
  | HBox of box
module House : sig
  val make_column : column -> house
  val make_row : row -> house
  val make_box : box -> house
  val comparer : house -> house -> int
  val to_string : house -> string
end
type houses = | CHouses of house list
module Houses : sig
  val drop : int -> houses -> houses
  val empty : houses
  val map : (house -> 'b) -> houses -> 'b list
  val mapi : (int -> house -> 'b) -> houses -> 'b list
  val ofList : house list -> houses
  val singleton : house -> houses
  val toList : houses -> house list
  val list_to_string : house list -> string
  val to_string : houses -> string
end
type digit = | Digit of char
module Digit : sig
  val make : int -> digit
  val comparer : digit -> digit -> int
  val to_string : digit -> string
end
type digits = | CDigits of digit list
module Digits : sig
  val contains : digit -> digits -> bool
  val count : digits -> int
  val difference : digits -> digits -> digits
  val empty : digits
  val filter : (digit -> bool) -> digits -> digits
  val first : digits -> digit
  val intersect : digits -> digits -> digits
  val isSubset : digits -> digits -> bool
  val nth : digits -> int -> digit
  val ofList : digit list -> digits
  val remove : digit -> digits -> digits
  val singleton : digit -> digits
  val toList : digits -> digit list
  val union : digits -> digits -> digits
  val unionManyList : digits list -> digits
  val list_to_string : digit list -> string
  val to_string : digits -> string
end
type puzzleShape =
  {size: size;
   boxWidth: boxWidth;
   boxHeight: boxHeight;
   alphabet: digits;}
module PuzzleShape : sig
    val default' : puzzleShape
end
type cellContents =
  | BigNumber of digit
  | PencilMarks of digits
module CellContents : sig
  val make_big_number : digit -> cellContents
  val make_pencil_marks : digits -> cellContents
end
type value =
  {cell: cell;
   digit: digit;}
module Value : sig
  val make : cell -> digit -> value
  val to_string : value -> string
end
type candidate =
  {cell: cell;
   digit: digit;}
module Candidate : sig
  val make : cell -> digit -> candidate
  val to_string : candidate -> string
end
type candidateReduction =
  {cell: cell;
   candidates: digits;}
module CandidateReduction : sig
  val make : cell -> digits -> candidateReduction
  val to_string : candidateReduction -> string
end
module CandidateReductions : sig
  val to_string : candidateReduction list -> string
end
type action =
  | Load of string
  | LoadEliminate
  | Placement of value
  | Eliminate of candidate
module Action : sig
  val to_string : action -> string
end
type given = Given of (cell * digit option) list
module Given : sig
    val get : given -> cell -> digit option
end
type current = Current of (cell * cellContents) list
module Current : sig
    val make : (cell * cellContents) list -> current
    val get : current -> cell -> cellContents
end
type cellCandidates = CellCandidates of (cell * digits) list
module CellCandidates : sig
    val get : cellCandidates -> cell -> digits
end
type solution =
  {given: given;
   current: current;
   steps: action list;}
module Solution : sig
    val givenToCurrent : cells -> given -> digits -> current
    val currentCellCandidates : cells -> current -> cellCandidates
end
