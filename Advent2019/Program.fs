// Learn more about F# at http://fsharp.org

open System
open System.IO;

let lines = File.ReadLines("./input/one.txt")


let fuelCount input:float = 
    let x = (input / 3.0) |> floor 
    x - 2.0

let rec fuelReq input:float =
    let newFuele = fuelCount input
    if newFuele > 0.0 then
        newFuele + (fuelReq newFuele)
    else
        0.0

let dayOne input = 
    let xx = input |> Seq.map (fun a -> float a |> fuelCount)
    xx |> Seq.sum

let dayTwo input = 
    let xx = input |> Seq.map (fun a -> float a |> fuelReq)
    xx |> Seq.sum

    // another comment

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    let frst = dayOne lines
    let snd = dayTwo lines

    0 // return an integer exit code
