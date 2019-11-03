module tests

open NUnit.Framework

[<SetUp>]
let Setup () =
    ()

[<Test>]
let Test1 () =
    let amountToTax pay limit roof = pay - limit |> min (roof - limit)
    let rateAmount (amount:double) (rate:double) = amount * rate
    let amountAfterTax pay limit roof rate = amountToTax pay limit roof |> rateAmount rate |> max 0.

    let minTax pay = amountAfterTax pay 10000. 30000. 0.1
    let midTax pay = amountAfterTax pay 30000. 100000. 0.25
    let highTax pay = amountAfterTax pay 100000. pay  0.4
    let totalTax pay = [minTax;midTax;highTax] |> List.map (fun x -> x pay)  |> List.sum
    let taxedAmount salary = salary |> totalTax

    Assert.That(totalTax 0.0, Is.EqualTo(0))
    Assert.That(totalTax 10009.0, Is.EqualTo(0.9))
    Assert.That(totalTax 10010.0, Is.EqualTo(1.))
    Assert.That(totalTax 12000.0 , Is.EqualTo(200.))
    Assert.That(totalTax 56789.0 , Is.EqualTo(8697.25))
    Assert.That(totalTax 1234567.0 |> int , Is.EqualTo(int 473326.8))
