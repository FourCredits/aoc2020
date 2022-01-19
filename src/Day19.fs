module Day19

open Utils.Parse

type Format =
    | Literal of char
    | Sum of int seq
    | Product of int seq * int seq

let parseRule (line: string) =
    let spaceSepNums (s: string) = s.Split " " |> Seq.map int
    let ruleNum = int (line[ .. line.IndexOf ':' - 1])
    let format =
        if line.Contains '"' then
            Literal (line[line.IndexOf '"' + 1])
        elif line.Contains '|' then
            match line[line.IndexOf ':' + 2 ..].Split " | " with
            | [| option1; option2 |] ->
                (spaceSepNums option1, spaceSepNums option2) |> Product
            | _ -> failwith "unexpected format"
        else
            (line[line.IndexOf ':' + 2 ..]).Split " " |> Seq.map int |> Sum
    (ruleNum, format)

let parse (lines: string array) =
    match Utils.split "" lines |> Seq.toList with
    | [ rules; messages ] -> (rules |> Seq.map parseRule |> Map.ofSeq, messages)
    | _ -> failwith "wrong format"

// Haskell's liftA2, specialised to sequences
let cartProd (reduction: 'a -> 'a -> 'a) (s1: 'a seq) (s2: 'a seq): 'a seq =
    [ for i1 in s1 do for i2 in s2 -> reduction i1 i2 ]

let makeRules rules =
    let mutable completed = Map.empty
    let options s =
        s
        |> Seq.map (fun i -> completed[i])
        |> Seq.reduce (cartProd (+))
        |> Seq.toList
    let allIn s = Seq.forall (fun i -> Map.containsKey i completed) s
    let finish = Map.count rules
    while Map.count completed < finish do
        rules
        |> Map.filter (fun k v ->
            (not (Map.containsKey k completed)) &&
                match v with
                | Literal _ -> true
                | Sum s -> allIn s
                | Product (option1, option2) -> allIn option1 && allIn option2)
        |> Map.map (fun _ -> function
            | Literal c -> List.singleton $"{c}"
            | Sum s -> options s
            | Product (o1, o2) -> options o1 @ options o2)
        |> Map.iter (fun k v -> completed <- Map.add k v completed)
    completed

let makeValidator2 (validMessages: Map<_, _>) =
    let fortyTwo = validMessages[42] |> Seq.map stringP |> anyOf |> countMany
    let thirtyOne = validMessages[31] |> Seq.map stringP |> anyOf |> countMany
    let (P p) =
        fortyTwo >>=? (fun (a, _) ->
            thirtyOne >>=? (fun (b, _) ->
                if a > b && a <> 0 && b <> 0 then ret () else fail))
        |> exactly
    (fun m -> match p m with | Some _ -> true | None -> false)

let main lines =
    let (rules, messages) = parse lines
    let validMessages = makeRules rules
    let validator1 m = Seq.contains m validMessages[0]
    let validator2 = makeValidator2 validMessages
    let part1 = messages |> Seq.filter validator1 |> Seq.length
    let part2 = messages |> Seq.filter validator2 |> Seq.length
    printfn $"{part1}, {part2}"
