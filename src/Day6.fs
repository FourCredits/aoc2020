module Day6

open Utils

let parse lines = lines |> split ""

let part1 = Seq.map (String.concat "" >> Set.ofSeq >> Set.count) >> Seq.sum

let part2 =
    Seq.map (Seq.map Set.ofSeq >> Set.intersectMany >> Set.count) >> Seq.sum

let main lines =
    let parsed = parse lines
    printfn $"{part1 parsed}, {part2 parsed}"
