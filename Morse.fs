module Morse
    let atoz = seq {'a'..'z'}
    let morse = ".- -... -.-. -.. . ..-. --. .... .. .--- -.- .-.. -- -. --- .--. --.- .-. ... - ..- ...- .-- -..- -.-- --.."; 
    let morsemap = morse.Split ' ' 

    let charToIndx (x:char) = 
        int x - int 'a' 
     
    let charToMorse (x:char) =
        charToIndx x |> morsemap.GetValue

    let stringToMorse (x:string) =
        x.ToCharArray() |> Array.map (fun y -> charToMorse y) |> Array.map string |> String.concat ""

    let rec positionsOf (x:string) (y:string) (sum:List<int>)= 
        let idx = y.IndexOf x
        if idx <> -1 then
            sum @ positionsOf x (y.Substring idx) sum
        else 
            sum

    let tes2t = positionsOf "a" "hejsan" []
            
            

