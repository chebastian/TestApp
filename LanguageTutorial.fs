module LanguageTutorial

type DayOne() =
    member this.input = System.IO.File.ReadAllLines("./input.txt")


let test = new DayOne()
printf "%A" test.input
