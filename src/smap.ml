module SMap = struct
    let ofLookup<'a, 'b when 'a : comparison> (as' : 'a list) (fn : 'a -> 'b) : ('a * 'b) list =
        List.map (fun a -> (a, fn a)) as'

    let get<'a, 'b when 'a : comparison> (as' : ('a * 'b) list) (k : 'a) : 'b =
        as'
        |> List.find (fst >> ((=) k))
        |> snd

    let tryGet<'a, 'b when 'a : comparison> (sm : ('a * 'b) list) (k : 'a) : 'b option =
        sm
        |> List.tryFind (fst >> ((=) k))
        |> Option.map snd

end
