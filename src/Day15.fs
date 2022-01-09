module Day15

let example = ["0,3,6"]

let turn l =
    match l with
    | n :: rest ->
        let n' =
            match List.tryFindIndex ((=) n) rest with
            | None -> 0
            | Some(index) -> index + 1
        n' :: l
    | _ -> failwith "unexpected"

let play n (s: int seq) =
    let mutable lastSpoken = Seq.last s
    let mutable lastSeen =
        s
        |> Seq.indexed
        |> Seq.map (fun (x, y) -> (y, x + 1))
        |> Map.ofSeq
        |> Map.remove lastSpoken
    [Seq.length s  + 1 .. n]
    |> Seq.fold (fun (lastSpoken, lastSeen) i ->
        ( match Map.tryFind lastSpoken lastSeen with
            | None -> 0
            | Some index -> (i - 1) - index
        , Map.add lastSpoken (i - 1) lastSeen)) (lastSpoken, lastSeen)
    |> fst

let part1 start = play 2020 start
let part2 start = play 30000000 start

let main (lines: string seq) =
    let parsed =
        lines |> Seq.item 0 |> (fun line -> line.Split ",") |> Seq.map int
    printfn $"{part1 parsed}, {part2 parsed}"
