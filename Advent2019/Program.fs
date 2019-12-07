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
    let lines = File.ReadLines("./input/2a.txt")
    type OPCode = 
    | Add = 1
    | Mul = 2
    | Error = -1
    | Halt = 99

    type InstructionResult(value:int,addr:int) = 
        member this.Value = value 
        member this.Addres = addr

    type Instruction = {Code:OPCode;Para:(int*int);Out:int}

    let delim = ','
    let instructionLength = 4

    let linesToInts (input:string) = 
        input.Split ',' |> Array.toSeq |> Seq.map (fun x -> int x)

    let mapLinesToInts (input:seq<string>) = 
        input |> Seq.item 0 |> linesToInts

    let seqIn = mapLinesToInts lines

    let GetOpCode input = 
        match input with
        | x::xs when x = 1 -> OPCode.Add
        | x::xs when x = 2 -> OPCode.Mul
        | x::xs when x = 99 -> OPCode.Halt
        | _ -> OPCode.Error

    let GetAdresses input = 
        match input with
        | [_;a;b;_] -> (int a, int b)
        | _ -> raise (new System.ArgumentException("No src and dest provided"))

    let GetOutAddr input =
        match input with
        | [_;_;_;addr] -> int addr
        | _ -> raise (new System.ArgumentException("No src and dest provided"))

    let GetValueAt (input:List<int>) (a,b) = 
        (int (input.Item a),int (input.Item b))

    let listWriteAt (input) addr a = List.mapi (fun i x -> if i = addr then a else x) input
    let strWriteAt (input) addr a = String.mapi (fun i x -> if i = addr then a else x) input
 
    let MapToCode input =
        let addr = GetAdresses input
        let outd = GetOutAddr input
        let op = GetOpCode input
        (op,addr,outd)


    let ResultOf input (instr:Instruction)= 
        let vals = GetValueAt input instr.Para
        match instr.Code with
        | OPCode.Add ->  new InstructionResult(((fst vals) + (snd vals)),instr.Out)
        | OPCode.Mul ->  new InstructionResult(((fst vals) * (snd vals)) ,instr.Out)
        | _ -> raise (new System.ArgumentException("OP Code not found"))


    let ReadInstruction i input=
        let len = (instructionLength * i)
        input |> Seq.skip len |> Seq.take instructionLength

    let GetInstruction i input =
        let inst = ReadInstruction i input
        {Code=GetOpCode(Seq.item 0 inst);Out=GetOutAddr(inst); Para=GetAdresses(inst)}

    let Apply input =
        let addr = GetAdresses input
        let outd = GetOutAddr input
        let vals = GetValueAt input addr
        let op = GetOpCode input
        match op with
        | OPCode.Add ->   ((fst vals) + (snd vals)) |> listWriteAt input outd 
        | OPCode.Mul ->  ((fst vals) * (snd vals)) |> listWriteAt input outd 
        | _ -> raise (new System.ArgumentException("OP Code not found"))
 
    let runProgram input = 
        0

    let rec runProgres input idx = 
        let instruction = ReadInstruction idx input
        if GetOpCode instruction = OPCode.Halt  then
            0
        else
            let res = InstructionResult instruction
        0

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    let frst = dayOne lines
    let snd = dayTwo lines

    0 // return an integer exit code
