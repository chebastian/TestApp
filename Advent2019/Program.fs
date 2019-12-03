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

module DayTwo  = 
    type OPCode = 
    | Add = 1
    | Mul = 2
    | Error = -1
    | Halt = 99

    let delim = ','

    let GetOpCode (input:string) = 
        match input.Split(delim) |> Array.toList with
        | x::xs when x = "1" -> OPCode.Add
        | x::xs when x = "2" -> OPCode.Mul
        | x::xs when x = "99" -> OPCode.Halt
        | _ -> OPCode.Error

    let GetAdresses (input:string) = 
        match input.Split(delim) |> Array.toList with
        | [_;a;b;_] -> (int a, int b)
        | _ -> raise (new System.ArgumentException("No src and dest provided"))

    let GetOutAddr (input:string) = 
        match input.Split(delim) |> Array.toList with
        | [_;_;_;addr] -> addr
        | _ -> raise (new System.ArgumentException("No src and dest provided"))

    let Apply (input:string) (op:OPCode) (rest:string)=
        let addr = GetAdresses input
        let outd = GetOutAddr input
        match op with
        | Add ->  0
        | _ -> 0

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    let frst = dayOne lines
    let snd = dayTwo lines

    0 // return an integer exit code
