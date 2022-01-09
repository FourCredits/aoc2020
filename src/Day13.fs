module Day13

let parse (lines: string seq) =
    let start = int64 (Seq.item 0 lines)
    let departures =
        (Seq.item 1 lines).Split ","
        |> Array.map (fun s -> if s = "x" then None else Some (int64 s))
    (start, departures)

let part1 (start, buses) =
    buses
    |> Seq.choose (Option.map (fun bus ->
        let earliest = Utils.iterate ((+) bus) 0L |> Seq.find ((<=) start)
        (earliest, bus)))
    |> Seq.min
    |> (fun (timestamp, busID) -> (timestamp - start) * busID)

let rec gcd a b = if b = 0L then a else gcd b (a % b)

let lcm a b = a * b / gcd a b

let part2 (_, buses) =
    let leaveDirectlyAfter x (earliest, inc) =
        let earliest' =
            Utils.iterate ((+) inc) earliest
            |> Seq.map (fun i -> i - 1L)
            |> Seq.find (fun i -> i % x = 0L)
        (earliest', lcm inc x)
    buses
    |> Seq.map (Option.defaultValue 1L)
    |> (fun i -> Seq.foldBack leaveDirectlyAfter i (1L, 1L))
    |> fst

let main lines =
    let parsed = parse lines
    printfn $"{part1 parsed}, {part2 parsed}"
