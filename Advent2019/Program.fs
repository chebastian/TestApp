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
    let inn = "1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,9,19,1,19,5,23,1,13,23,27,1,27,6,31,2,31,6,35,2,6,35,39,1,39,5,43,1,13,43,47,1,6,47,51,2,13,51,55,1,10,55,59,1,59,5,63,1,10,63,67,1,67,5,71,1,71,10,75,1,9,75,79,2,13,79,83,1,9,83,87,2,87,13,91,1,10,91,95,1,95,9,99,1,13,99,103,2,103,13,107,1,107,10,111,2,10,111,115,1,115,9,119,2,119,6,123,1,5,123,127,1,5,127,131,1,10,131,135,1,135,6,139,1,10,139,143,1,143,6,147,2,147,13,151,1,5,151,155,1,155,5,159,1,159,2,163,1,163,9,0,99,2,14,0,0"
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
        (Seq.item 1 input,Seq.item 2 input)

    let GetOutAddr (input:seq<int>) :int =
        Seq.item 3 input

    let GetValueAt (input:list<int>) (a,b) = 
        (int (input.Item a),int (input.Item b))

    let listWriteAt (input) addr a = Seq.mapi (fun i x -> if i = addr then a else x) input
    let strWriteAt (input) addr a = String.mapi (fun i x -> if i = addr then a else x) input

    let WriteResult input (result:InstructionResult) =
        listWriteAt input result.Addres result.Value
        
 
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


    let ReadInstruction i input =
        let len = (instructionLength * i)
        input |> Seq.skip len |> Seq.take instructionLength

    let NumberOfInstructions ins =
        Seq.length |> ins |> (/) 4

    let ReadValue input addr =
        Seq.item addr 

    let GetInstruction i myin =
        let inst = ReadInstruction i myin
        let outDD = GetOutAddr inst
        let opt = inst |> Seq.toList |> GetOpCode
        let par = GetAdresses inst
        {Code=opt;Out=outDD; Para=par} 

    let Apply input =
        let addr = GetAdresses input
        let outd = GetOutAddr input
        let vals = GetValueAt input addr
        let op = GetOpCode input
        match op with
        | OPCode.Add ->   ((fst vals) + (snd vals)) |> listWriteAt input outd 
        | OPCode.Mul ->  ((fst vals) * (snd vals)) |> listWriteAt input outd 
        | _ -> raise (new System.ArgumentException("OP Code not found"))
 
    let runProgram input verb noun = 
        let frst = Seq.head input 
        let rest = Seq.skip 3 input
        //let ins = linesToInts inn 
        let ins = frst :: verb :: noun :: Seq.toList rest
        let len =  (Seq.length ins ) / 4

        let rec Program instructionPtr memory =
            let code = GetInstruction instructionPtr memory
            if code.Code = OPCode.Halt then
                Seq.item 0 memory
            else 
                let res = ResultOf (Seq.toList memory) code
                let ret = WriteResult memory res |> Program (instructionPtr+1)
                ret

        Program 0 ins

[<EntryPoint>]
let main argv =
    let ins = DayTwo.linesToInts DayTwo.inn

    for x in [0..100] do
        for y in [0..100] do 
            let result = DayTwo.runProgram ins x y
            //printf "%A" result
            if result = 19690720 then
                (x,y)
            else
                (0,0)


    printfn "Hello World from F#!"
    let frst = dayOne lines
    let snd = dayTwo lines 

    0 // return an integer exit code
