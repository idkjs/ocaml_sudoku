let ofLookup (as' : 'a list) (fn : 'a -> 'b) : ('a * 'b) list =
    List.map (fun a -> (a, fn a)) as'

let get (comparer : 'a -> 'a -> int) (as' : ('a * 'b) list) (k : 'a) : 'b =
    as'
    |> List.find (fun a -> comparer k (fst a) = 0)
    |> snd

let tryGet (comparer : 'a -> 'a -> int) (sm : ('a * 'b) list) (k : 'a) : 'b option =
    if List.exists (fun a -> comparer k (fst a) = 0) sm then Some (get comparer sm k)
    else None
