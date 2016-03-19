module core.smap

open System
open System.Collections
open System.Collections.Generic

type SMap<'T, 'U when 'T : comparison> =
    {
        data : Map<'T, 'U>
    }

module SMap =
    let ofList<'a, 'b when 'a : comparison> (as' : ('a * 'b) list) : SMap<'a, 'b> =
        {
            data = 
                as'
                |> Map.ofList
        }

    let ofLookup<'a, 'b when 'a : comparison> (as' : 'a list) (fn : 'a -> 'b) : SMap<'a, 'b> =
        {
            data = 
                as'
                |> List.map (fun a -> (a, fn a))
                |> Map.ofSeq
        }

    let get<'a, 'b when 'a : comparison> (sm : SMap<'a, 'b>) (a : 'a) : 'b =
        sm.data.Item a

    let tryGet<'a, 'b when 'a : comparison> (sm : SMap<'a, 'b>) (a : 'a) : 'b option =
        sm.data.TryFind a
(*
    let get<'a, 'b when 'a : comparison> (as' : ('a * 'b) list) (k : 'a) : 'b =
        as'
        |> List.find (fst >> ((=) k))
        |> snd
*)