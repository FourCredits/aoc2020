module Day18

open Utils.Parse

#nowarn "40"
let part1 lines =
    let rec exprParser =
        let recur = P (fun s -> parse exprParser s)
        let brackets = (surround (stringP "(") (stringP ")") recur)
        let operand = intP <|>? brackets
        let plus = (fun _ -> (+)) <?> charP '+'
        let mult = (fun _ -> (*)) <?> charP '*'
        let operation = (ws >>? (plus <|>? mult) <<? ws) <*>? operand
        (|>) <?> operand <*>? ((Seq.fold (>>) id) <?> many operation)
    lines |> Seq.map (parse exprParser >> Option.get >> fst) |> Seq.sum

let findMatching (s: string) (n: int): int =
    let rec find depth position =
        if position > s.Length then failwith "no matching parenthesis"
        else
            let depth' =
                match s[position] with
                | '(' -> depth + 1
                | ')' -> depth - 1
                | _ -> depth
            if depth' = 0 then position else find depth' (position + 1)
    if s[n] <> '(' then failwith "character under cursor must be `(`"
    else find 1 (n + 1)

let part2 lines =
    let rec eval (s: string): int64 =
        let s = s.Trim()
        match s.IndexOf('(') with
        | -1 ->
            s.Split " * "
            |> Array.map (fun part ->
                part.Split " + " |> Array.map int64 |> Array.reduce (+))
            |> Array.reduce (*)
        | op ->
            let cl = findMatching s op
            let lh: string = s[..op - 1].Trim()
            let rh: string = s[cl + 1..].Trim()
            let middle: int64 = eval s[op + 1 .. cl - 1]
            eval (sprintf "%s %i %s" lh middle rh)
    lines |> Seq.map eval |> Seq.sum

let main lines = printfn $"{part1 lines}, {part2 lines}"
