module Taxes
    let amountToTax pay limit roof = pay - limit |> min (roof - limit)
    let rateAmount (amount:double) (rate:double) = amount * rate
    let amountAfterTax pay limit roof rate = amountToTax pay limit roof |> rateAmount rate |> max 0.

    let minTax pay = amountAfterTax pay 10000. 30000. 0.1
    let midTax pay = amountAfterTax pay 30000. 100000. 0.25
    let highTax pay = amountAfterTax pay 100000. pay  0.4
    let totalTax pay = [minTax;midTax;highTax] |> List.map (fun x -> x pay)  |> List.sum
    let taxedAmount salary = salary |> totalTax

