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

    let findResult =
        let ins = linesToInts inn
        for x in [0..100] do
            for y in [0..100] do 
                let result = runProgram ins x y
                if result = 19690720 then
                    (x,y)
                else
                    (0,0)


module DayThree =

    let lines = File.ReadLines("./input/3.txt")

    type Direction = |Left|Up|Down|Right
    type DayInput = {D:Direction;Len:int}

    let inputA = "R998,U547,L703,D251,L776,U837,R100,U240,R197,D216,L220,U606,L437,U56,R940,U800,L968,D464,L870,D797,L545,D824,R790,U5,R347,D794,R204,U538,L247,U385,L103,D260,L590,U813,L549,U309,L550,U321,R862,D686,R368,D991,R451,D836,R264,D138,L292,D319,L784,D369,R849,U865,R776,D726,R223,D118,L790,D208,L836,D592,R310,D36,R991,U674,L205,U407,R422,U350,L126,D320,L239,U353,L509,U48,R521,D544,L157,D551,R614,D493,R407,D965,R498,U248,R826,U573,L782,D589,R616,D992,L806,D745,R28,U142,L333,D849,L858,D617,R167,U341,R46,U940,L615,D997,L447,D604,R148,U561,R925,D673,R441,U200,R458,U193,L805,D723,L208,U600,L926,U614,R660,D183,L408,D834,R248,U354,L110,U391,L37,U599,L287,U28,R859,D936,L404,D952,R11,U20,R708,U218,L800,U750,R936,D213,R6,D844,R728,D391,R114,U406,R390,U791,L199,D397,R476,D583,R99,U419,R575,D836,L896,U780,L77,U964,R441,U723,R248,D170,R527,D94,L39,U645,L338,D728,R503,U641,L358,D287,R171,U368,R176,D986,R821,U912,L231,D206,L451,U900,L35,D490,R190,D180,L937,D500,R157,U989,L336,U202,R178,U52,R931,U306,L85,D866,R756,U715,L521,D977,R936,U4,R207,D384,L785,U138,L682,U488,L537,U250,L877,D446,R849,U35,R258,U784,R263,D494,L324,U601,R302,U473,L737,D143,R184,D967,R95,U51,L713,U733,R297,U740,R677,D715,R750,U143,L980,U260,R915,D535,R202,U460,R365,U956,L73,U441,R182,D982,L869,D755,L837,D933,L856,D341,R189,D519,L387,D144,R575,U682,R317,U838,R154,D201,R237,D410,L43,U853,L495,U983,L953,U220,R697,D592,R355,U377,R792,U824,L441,U783,R258,D955,R451,D178,L151,D435,L232,U923,L663,U283,L92,D229,R514"

    //let inputToPInput (input:string) = 
    //    input.Split(',')  |> Array.map (fun item -> )
        
    let strToDayInput (str:string) =
        let dd = match str.Chars 0 with 
                | 'R' -> Direction.Right
                | 'L' -> Direction.Left
                | 'U' -> Direction.Up
                | 'D' -> Direction.Down
                | _ -> Direction.Up

        let len = str.ToCharArray() |> Array.skip 1 |> string |> int
        {D=dd;Len=len}

    let test = strToDayInput "D123"
    type Dir =
    | Hor
    | Vert


    type Line = {x1:int;y1:int;x2:int;y2:int;}

    type PInput = {line:Line;d:Dir}

    type Vector = {x:float;y:float}

    let aline = {x1=1;x2=5;y1=1;y2=1}
    let aline2 = {x1=3;x2=3;y1=1;y2=3}
    let bline = {x1=6;x2=6;y1=1;y2=6}
    let leftline = {x1=8;x2=2;y1=1;y2=1}
    let upline = {x1=2;x2=2;y1=9;y2=1}

    let apin = {line=aline;d=Dir.Hor}
    let bpin = {line=bline;d=Dir.Vert}
    let cline = {line=leftline;d=Dir.Hor}
    let dline = {line=upline;d=Dir.Vert}
    let iline = {line=aline2;d=Dir.Vert}

    let lineDir (line:Line) =
        {x=float(line.x1-line.x2);y=float(line.y1 - line.y2)}

    let linePoints (a:PInput) =

        let aminbmax  a1 a2 = 
            (min a1 a2, max a1 a2)

        let xrange = aminbmax a.line.x1 a.line.x2
        let yrange = aminbmax a.line.y1 a.line.y2

        if a.d = Dir.Hor then
            let xs = [fst xrange..snd xrange]
            List.map (fun x -> (x,a.line.y1)) xs
        else
            let xs = [fst yrange..snd yrange]
            List.map (fun y -> (a.line.x1,y)) xs

    let lineIntersectionPoint (a:PInput, b:PInput)  = 
        let ap = linePoints a
        let bp = linePoints b 
        List.where (fun x -> List.contains x ap) bp

 
[<EntryPoint>]
let main argv =
    let ins = DayTwo.linesToInts DayTwo.inn

    for x in [0..100] do
        for y in [0..100] do 
            let result = DayTwo.runProgram ins x y
            if result = 19690720 then
                (x,y)
            else
                (0,0)


    printfn "Hello World from F#!"
    let frst = dayOne lines
    let snd = dayTwo lines 

    0 // return an integer exit code
