module core.sset

open System
open System.Collections
open System.Collections.Generic

type SSet<'T when 'T : comparison> =
    {
        data : 'T list
    }
    interface IEnumerable with
        member x.GetEnumerator(): IEnumerator = 
            failwith "Not implemented yet"
    interface IEnumerable<'T> with
        member this.GetEnumerator() = (Seq.cast<'T> this.data).GetEnumerator()

module SSet =
    let empty<'T when 'T : comparison> : SSet<'T> = { data = []} 

    let count<'T when 'T : comparison> (s : SSet<'T>) : int = s.data.Length

    let map<'T, 'U when 'T : comparison and 'U : comparison> (map : 'T -> 'U) (s : SSet<'T>) : SSet<'U> = { data = List.map map s.data }

    let add<'T when 'T : comparison> (t : 'T) (s : SSet<'T>) =
        if List.exists (fun t' -> t' = t) s.data then s
        else { data = t :: s.data}

    let contains<'T when 'T : comparison> (t : 'T) (s : SSet<'T>) : bool = List.exists (fun t' -> t' = t) s.data

    let filter<'T when 'T : comparison> (predicate : 'T -> bool) (s : SSet<'T>) : SSet<'T> = { data = List.filter predicate s.data }

    let ofArray<'T when 'T : comparison> (ts : 'T array) : SSet<'T> =
        Array.fold (fun set elem -> add elem set) empty ts

    let remove<'T when 'T : comparison> (t : 'T) (s : SSet<'T>) =
        { data = List.filter (fun t' -> t' <> t) s.data }

    let union<'T when 'T : comparison> (ts : SSet<'T>) (ts' : SSet<'T>) = ofArray ((List.append ts.data ts'.data) |> List.toArray)

    let toArray<'T when 'T : comparison> (ts : SSet<'T>) : 'T array = ts.data |> Array.ofList

    let unionMany<'T when 'T : comparison> (tss : seq<SSet<'T>>) : SSet<'T> =
        Seq.collect (fun ts -> ts.data) tss |> Seq.toArray |> ofArray

    let iter<'T when 'T : comparison> (fn) (ts : SSet<'T>) = ts.data :> IEnumerable<'T> |> Seq.iter fn

    let intersect<'T when 'T : comparison> (ts : SSet<'T>) (ts' : SSet<'T>) =
        Set.intersect (ts.data |> Set.ofList) (ts'.data |> Set.ofList)
        |> Seq.toArray
        |> ofArray

    let difference<'T when 'T : comparison> (ts : SSet<'T>) (ts' : SSet<'T>) =
        Set.difference (ts.data |> Set.ofList) (ts'.data |> Set.ofList)
        |> Seq.toArray
        |> ofArray

    let isSubset<'T when 'T : comparison> (ts : SSet<'T>) (ts' : SSet<'T>) =
        Set.isSubset (ts.data |> Set.ofList) (ts'.data |> Set.ofList)
     