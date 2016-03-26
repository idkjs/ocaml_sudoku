(*
Sset is intended to be an ordered list, without duplicates.

Bits borrowed from:
https://www.cl.cam.ac.uk/~jrh13/atp/OCaml/lib.ml
https://hackage.haskell.org/package/base-4.8.2.0/docs/src/GHC.List.html
*)

let rec drop<'a> (n : int) (l : 'a list) : 'a list =
if n <= 0 then l
else
    match l with
    | [] -> []
    | _::t  when n = 1 -> t
    | h::t -> drop (n-1) t


let rec uniq<'a> (comparer : 'a -> 'a -> int) (l : 'a list) : 'a list =
    match l with
    | [] -> []
    | x :: y :: tl when comparer x y = 0 -> x :: (uniq comparer tl)
    | hd :: tl -> hd :: (uniq comparer tl)

let normalise<'a> (comparer : 'a -> 'a -> int) (l : 'a list) : 'a list =
    l
    |> uniq comparer
    |> List.sortWith comparer

(* ------------------------------------------------------------------------- *)
(* Merging of sorted lists (maintaining repetitions).                        *)
(* ------------------------------------------------------------------------- *)

let rec merge ord l1 l2 =
  match l1 with
    [] -> l2
  | h1::t1 -> match l2 with
                [] -> l1
              | h2::t2 -> if ord h1 h2 then h1::(merge ord t1 l2)
                          else h2::(merge ord l1 t2)

let map f =
  let rec mapf l =
    match l with
      [] -> []
    | (x::t) -> let y = f x in y::(mapf t) in
  mapf;;

(* ------------------------------------------------------------------------- *)
(* Bottom-up mergesort.                                                      *)
(* ------------------------------------------------------------------------- *)

let sort ord =
  let rec mergepairs l1 l2 =
    match (l1,l2) with
        ([s],[]) -> s
      | (l,[]) -> mergepairs [] l
      | (l,[s1]) -> mergepairs (s1::l) []
      | (l,(s1::s2::ss)) -> mergepairs ((merge ord s1 s2)::l) ss in
  fun l -> if l = [] then [] else mergepairs [] (map (fun x -> [x]) l);;

let setify (comparer : 'a -> 'a -> int) (l : 'a list) : 'a list =
  let rec canonical lis =
     match lis with
       x::y::_ as rest -> comparer x y < 0 && canonical rest
     | _ -> true in

  if canonical l then l
  else uniq comparer (sort (fun x y -> comparer x y <= 0) l)

let union (comparer : 'a -> 'a -> int) =
  let rec union l1 l2 =
    match (l1,l2) with
        ([],l2) -> l2
      | (l1,[]) -> l1
      | ((h1::t1 as l1),(h2::t2 as l2)) ->
          if h1 = h2 then h1::(union t1 t2)
          else if h1 < h2 then h1::(union t1 l2)
          else h2::(union l1 t2) in
  fun s1 s2 -> union (setify comparer s1) (setify comparer s2);;

let intersect (comparer : 'a -> 'a -> int) =
  let rec intersect l1 l2 =
    match (l1,l2) with
        ([],l2) -> []
      | (l1,[]) -> []
      | ((h1::t1 as l1),(h2::t2 as l2)) ->
          if h1 = h2 then h1::(intersect t1 t2)
          else if h1 < h2 then intersect t1 l2
          else intersect l1 t2 in
  fun s1 s2 -> intersect (setify comparer s1) (setify comparer s2);;

let subtract (comparer : 'a -> 'a -> int) =
  let rec subtract l1 l2 =
    match (l1,l2) with
        ([],l2) -> []
      | (l1,[]) -> l1
      | ((h1::t1 as l1),(h2::t2 as l2)) ->
          if h1 = h2 then subtract t1 t2
          else if h1 < h2 then h1::(subtract t1 l2)
          else subtract l1 t2 in
  fun s1 s2 -> subtract (setify comparer s1) (setify comparer s2);;

(*
let subset,psubset =
  let rec subset l1 l2 =
    match (l1,l2) with
        ([],l2) -> true
      | (l1,[]) -> false
      | (h1::t1,h2::t2) ->
          if h1 = h2 then subset t1 t2
          else if h1 < h2 then false
          else subset l1 t2
  and psubset l1 l2 =
    match (l1,l2) with
        (l1,[]) -> false
      | ([],l2) -> true
      | (h1::t1,h2::t2) ->
          if h1 = h2 then psubset t1 t2
          else if h1 < h2 then false
          else subset l1 t2 in
  (fun s1 s2 -> subset (setify comparer s1) (setify comparer s2)),
  (fun s1 s2 -> psubset (setify comparer s1) (setify comparer s2));;
*)
let insert (comparer : 'a -> 'a -> int) x s = union comparer [x] s;;

let image (comparer : 'a -> 'a -> int) f s = setify comparer (map f s);;

(* ------------------------------------------------------------------------- *)
(* Union of a family of sets.                                                *)
(* ------------------------------------------------------------------------- *)
let rec itlist f l b =
  match l with
    [] -> b
  | (h::t) -> f h (itlist f t b);;

let unions comparer s = setify comparer (itlist (@) s []);;










let contains<'a> (comparer : 'a -> 'a -> int) (t : 'a) (s : 'a list) : bool =
    List.exists (fun t' -> comparer t' t = 0) s

let remove<'a> (comparer : 'a -> 'a -> int) (t : 'a) (s : 'a list) : 'a list =
    List.filter (fun t' -> comparer t' t <> 0) s

let ofList<'a> (comparer : 'a -> 'a -> int) (xs : 'a list) : 'a list =
    xs
    |> normalise comparer

let isSubset<'a when 'a : comparison> (ts : 'a list) (ts' : 'a list) : bool =
    Set.isSubset (ts |> Set.ofList) (ts' |> Set.ofList)

let rec doSetSubsets (list : 'a list) (size : int) (prefix : 'a list) : 'a list list = 
    match list with
    | x :: xs when size > 0 -> 
        if size = 1 then (x :: prefix) :: doSetSubsets xs 1 prefix
        else 
            let inc = doSetSubsets xs (size - 1) (x :: prefix) in
            let dec = doSetSubsets xs size prefix in

            List.append inc dec
    | _ -> []

let rec setSubsets (as' : 'a list) (size : int) : 'a list list =
    doSetSubsets as' size []

(*
    let s0 = []
    let p00 = setSubsets s0 0
    let p01 = setSubsets s0 1
    let p02 = setSubsets s0 2

    let s1 = [ 1 ]
    let p10 = setSubsets s1 0
    let p11 = setSubsets s1 1
    let p12 = setSubsets s1 2
    let p13 = setSubsets s1 3

    let s2 = [ 1; 2 ]
    let p20 = setSubsets s2 0
    let p21 = setSubsets s2 1
    let p22 = setSubsets s2 2
    let p23 = setSubsets s2 3
    let p24 = setSubsets s2 4

    let s3 = [ 1; 2; 3 ]
    let p30 = setSubsets s3 0
    let p31 = setSubsets s3 1
    let p32 = setSubsets s3 2
    let p33 = setSubsets s3 3
    let p34 = setSubsets s3 4
    let p35 = setSubsets s3 5
*)
