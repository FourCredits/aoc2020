module Day10

let count x = Seq.filter ((=) x) >> Seq.length

let parse lines =
    let adapters = Seq.map int lines
    adapters
    |> Seq.insertManyAt 0 [0; Seq.max adapters + 3]
    |> Seq.sortDescending

let part1 adapters =
    let differences = adapters |> Seq.pairwise |> Seq.map (fun (x, y) -> x - y)
    count 1 differences * count 3 differences

let part2 adapters =
    let device = Seq.head adapters
    let mutable memo = Map [(device, (int64 1))]
    for adapter in Seq.tail adapters do
        let combinations =
            [ for next in [adapter + 1 .. adapter + 3] do
                if Map.containsKey next memo then memo[next] ]
            |> Seq.reduce (+)
        memo <- Map.add adapter combinations memo
    memo[0]

let main (lines:string seq) =
    let parsed = parse lines
    printfn $"{part1 parsed}, {part2 parsed}"
