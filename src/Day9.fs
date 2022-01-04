module Day9

let part1 (numbers:int64 array) =
    let windowSize = 40
    [| for i = windowSize to Array.length numbers - 1 do
        let previous = numbers[i - windowSize .. i - 1]
        let valid (x, y) = x < y && x + y = numbers[i]
        if not (Array.allPairs previous previous |> Array.exists valid)
            then numbers[i]
    |]
    |> Array.head

let part2 numbers =
    let invalid = part1 numbers
    let l = Array.length numbers
    [| for i = 0 to l - 2 do
        for j = i + 1 to l - 1 do
            let window = numbers[i .. j]
            if Array.sum window = invalid then
                Array.min window + Array.max window |]
    |> Array.exactlyOne

let main (lines:string seq) =
    let parsed = lines |> Seq.map int64 |> Seq.toArray
    printfn $"{part1 parsed}, {part2 parsed}"
