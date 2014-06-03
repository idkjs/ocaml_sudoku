module shell

open System

let getInput (prompt:string) =
    Console.Write prompt
    Console.ReadLine()

let readlines = Seq.initInfinite (fun _ -> getInput(">"))

