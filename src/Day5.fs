module Day5

let parser line =
    let f = function
        | 'F' -> 0
        | 'B' -> 1
        | 'L' -> 0
        | 'R' -> 1
        | _   -> failwith "unrecognized character"
    let line' = Seq.map f line
    let rowPart = Seq.take 7 line'
    let colPart = Seq.skip 7 line'
    (rowPart, colPart)

let bitsToInt = Seq.fold (fun acc bit -> 2 * acc + bit) 0

let seatID (r, c) = (8 * bitsToInt r) + bitsToInt c

let part1 = Seq.map seatID >> Seq.max

let part2 passes =
    let ids = Seq.map seatID passes
    let ours i =
        not (Seq.contains i ids) &&
        Seq.contains (i + 1) ids &&
        (Seq.contains (i - 1) ids)
    Seq.filter ours [ 0 .. 1023 ]
    |> Seq.head

let main lines =
    let parsed = Seq.map parser lines
    printfn $"{part1 parsed}, {part2 parsed}"
