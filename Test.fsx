let atoz = seq {'a'..'z'}
let morse = ".- -... -.-. -.. . ..-. --. .... .. .--- -.- .-.. -- -. --- .--. --.- .-. ... - ..- ...- .-- -..- -.-- --.."; 
let morsemap = morse.Split ' ' 

let charToIndx (x:char) = 
    int x - int 'a' 
 
let charToMorse (x:char) =
    charToIndx x |> morsemap.GetValue

let stringToMorse (x:string) =
    x.ToCharArray() |> Array.map (fun y -> charToMorse y)



charToIndx 'b'
stringToMorse "hello"

let this="amazeballs"

let taxedAmount limit roof (rate:float) pay = if pay > limit then min roof (pay - limit) * rate else 0.0
