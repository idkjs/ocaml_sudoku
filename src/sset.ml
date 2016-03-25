(*
Sset is intended to be an ordered list, without duplicates.
*)

let rec drop<'a> (n : int) (l : 'a list) : 'a list =
if n <= 0 then l
else
    match l with
    | [] -> []
    | _::t  when n = 1 -> t
    | h::t -> drop (n-1) t

let add<'a when 'a : comparison> (t : 'a) (s : 'a list) : 'a list=
    if List.exists (fun t' -> t' = t) s then s
    else t :: s

let contains<'a when 'a : comparison> (t : 'a) (s : 'a list) : bool =
    List.exists (fun t' -> t' = t) s

let remove<'a when 'a : comparison> (t : 'a) (s : 'a list) : 'a list =
    List.filter (fun t' -> t' <> t) s

let ofList<'a when 'a : comparison> (xs : 'a list) : 'a list =
    xs
    |> Set.ofList
    |> Set.toList

let union<'a when 'a : comparison> (ts : 'a list) (ts' : 'a list) : 'a list =
    (List.append ts ts')
    |> ofList

let unionMany<'a when 'a : comparison> (tss : 'a list list) : 'a list =
    tss
    |> List.concat
    |> ofList

let intersect<'a when 'a : comparison> (ts : 'a list) (ts' : 'a list) : 'a list =
    Set.intersect (ts |> Set.ofList) (ts' |> Set.ofList)
    |> Seq.toList

let difference<'a when 'a : comparison> (ts : 'a list) (ts' : 'a list) : 'a list =
    Set.difference (ts |> Set.ofList) (ts' |> Set.ofList)
    |> Seq.toList

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
