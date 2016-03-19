(*F#
module sset

open System
open System.Collections
open System.Collections.Generic
F#*)

(*F#
type SSet<'T when 'T : comparison> =
    { data : 'T list }

module SSet =
    let empty<'T when 'T : comparison> : SSet<'T> = { data = []} 

    let count<'T when 'T : comparison> (s : SSet<'T>) : int = s.data.Length

    let map<'T, 'U when 'T : comparison and 'U : comparison> (map : 'T -> 'U) (s : SSet<'T>) : SSet<'U> = { data = List.map map s.data }

    let add<'T when 'T : comparison> (t : 'T) (s : SSet<'T>) =
        if List.exists (fun t' -> t' = t) s.data then s
        else { data = t :: s.data}

    let contains<'T when 'T : comparison> (t : 'T) (s : SSet<'T>) : bool = List.exists (fun t' -> t' = t) s.data

    let filter<'T when 'T : comparison> (predicate : 'T -> bool) (s : SSet<'T>) : SSet<'T> = { data = List.filter predicate s.data }

    let ofList<'T when 'T : comparison> (ts : 'T list) : SSet<'T> =
        List.fold (fun set elem -> add elem set) empty ts

    let remove<'T when 'T : comparison> (t : 'T) (s : SSet<'T>) =
        { data = List.filter (fun t' -> t' <> t) s.data }

    let union<'T when 'T : comparison> (ts : SSet<'T>) (ts' : SSet<'T>) = ofList ((List.append ts.data ts'.data))

    let toList<'T when 'T : comparison> (ts : SSet<'T>) : 'T list = ts.data

    let unionMany<'T when 'T : comparison> (tss : seq<SSet<'T>>) : SSet<'T> =
        Seq.collect (fun ts -> ts.data) tss |> Seq.toList |> ofList

    let iter<'T when 'T : comparison> (fn) (ts : SSet<'T>) = ts.data :> IEnumerable<'T> |> Seq.iter fn

    let intersect<'T when 'T : comparison> (ts : SSet<'T>) (ts' : SSet<'T>) =
        Set.intersect (ts.data |> Set.ofList) (ts'.data |> Set.ofList)
        |> Seq.toList
        |> ofList

    let difference<'T when 'T : comparison> (ts : SSet<'T>) (ts' : SSet<'T>) =
        Set.difference (ts.data |> Set.ofList) (ts'.data |> Set.ofList)
        |> Seq.toList
        |> ofList

    let isSubset<'T when 'T : comparison> (ts : SSet<'T>) (ts' : SSet<'T>) =
        Set.isSubset (ts.data |> Set.ofList) (ts'.data |> Set.ofList)
F#*)
