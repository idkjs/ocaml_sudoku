let ofLookup (fn : 'a -> 'b) (as' : 'a list) : ('a * 'b) list =
    List.map (fun a -> (a, fn a)) as'

let get (comparer : 'a -> 'a -> int) (k : 'a) (as' : ('a * 'b) list) : 'b =
    as'
    |> List.find (fun a -> comparer k (fst a) = 0)
    |> snd

let tryGet (comparer : 'a -> 'a -> int) (k : 'a) (sm : ('a * 'b) list) : 'b option =
    if List.exists (fun a -> comparer k (fst a) = 0) sm then Some (get comparer k sm)
    else None
