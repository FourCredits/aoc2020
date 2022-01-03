module Day7

let parse (line:string) =
    let words = line.Split " "
    let bag = String.concat " " words[0 .. 1]
    let rest = String.concat " " words[4 .. ]
    let bagsS = rest.Split ", "
    let bags =
        if bagsS = [| "no other bags." |] then
            Seq.empty
        else
            let getDesc (s:string) =
                let words = s.Split " "
                (int words[0], String.concat " " words[1 .. 2])
            Seq.map getDesc bagsS
    (bag, bags)

let part1 (rules:Map<_, _>) =
    let rec canContain inner outer =
        let innerBags = rules[outer] |> Seq.map snd
        Seq.contains inner innerBags || Seq.exists (canContain inner) innerBags
    rules
    |> Map.filter (fun k _ -> canContain "shiny gold" k)
    |> Map.count

let part2 (rules:Map<_,_>) =
    let rec numBags outer =
        Seq.sum [for (n, subBag) in rules[outer] -> n * (1 + numBags subBag)]
    numBags "shiny gold"

let main lines =
    let parsed = lines |> Seq.map parse |> Map.ofSeq
    printfn $"{part1 parsed}, {part2 parsed}"
