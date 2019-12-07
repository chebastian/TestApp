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
    let instructionLength = 4

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
        | [_;_;_;addr] -> int addr
        | _ -> raise (new System.ArgumentException("No src and dest provided"))

    let GetValueAt (input:string) (a,b) = 
        let split =  input.Split(delim) |> Array.toList 
        (int (split.Item a),int (split.Item b))

    let listWriteAt (input) addr a = List.mapi (fun i x -> if i = addr then a else x) input
    let strWriteAt (input) addr a = String.mapi (fun i x -> if i = addr then a else x) input
 
    let MapToCode (input:string) = 
        let addr = GetAdresses input
        let outd = GetOutAddr input
        let op = GetOpCode input
        (op,addr,outd)

    let ReadInstruction (input:string) i=
        let split =  input.Split(delim) |> Array.toList 
        let len = (instructionLength * i)
        split |> List.skip len |> List.take instructionLength


    let Apply (input:string) =
        let split =  input.Split(delim) |> Array.toList 
        let addr = GetAdresses input
        let outd = GetOutAddr input
        let vals = GetValueAt input addr
        let op = GetOpCode input
        match op with
        | OPCode.Add ->  string ((fst vals) + (snd vals)) |> listWriteAt split outd 
        | OPCode.Mul -> string ((fst vals) * (snd vals)) |> listWriteAt split outd 
        | _ -> split

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    let frst = dayOne lines
    let snd = dayTwo lines

    0 // return an integer exit code
