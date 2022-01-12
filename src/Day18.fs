module Day18

let undefined () = failwith "undefined"

let first f (a, b) = (f a, b)

let second f (a, b) = (a, f b)

[<AutoOpenAttribute>]
module Parse =
    type 'a Parser = P of (string -> ('a * string) option)

    let parse (P p) s = p s

    let (<?>) f (P p) = P (fun s -> p s |> Option.map (first f))

    let ret a = P (fun s -> Some (a, s))

    let (>>=?) (P pa: 'a Parser) (pm: ('a -> 'b Parser)): 'b Parser =
        P (fun s -> pa s |> Option.bind (fun (a, rest) -> parse (pm a) rest))

    let (>>?) l p = l >>=? (fun _ -> p)

    let (<<?) p r = p >>=? (fun v -> r >>? (ret v))

    let (<*>?) pf pa = pf >>=? (fun f -> f <?> pa)

    let (<|>?) (P p1) (P p2) = P (fun s -> Option.orElse (p2 s) (p1 s))

    let map = (<?>)

    let join = (<*>?)

    let bind pm pa = pa >>=? pm

    let alt = (<|>?)

    let longest (P p1) (P p2) =
        P (fun s ->
            match (p1 s, p2 s) with
            | (None, None) -> None
            | (None, Some v2) -> Some v2
            | (Some v1, None) -> Some v1
            | (Some v1, Some v2) ->
                Some (Seq.minBy (snd >> Seq.length) [v1; v2]))

    let rec sequence parsers =
        match parsers with
        | [] -> ret []
        | p :: ps -> ((fun v vs -> v :: vs) <?> p) <*>? (sequence ps)

    let get: char Parser =
        P (fun s -> if s = "" then None else Some(s[0], s[1..]))

    let peek: char Parser =
        P (fun s -> if s = "" then None else Some(s[0], s))
    
    let charP c = P (fun s -> parse get s |> Option.filter (fst >> (=) c))

    let stringP (s: string) =
        s
        |> Seq.map charP
        |> Seq.toList
        |> sequence
        |> map System.String.Concat

    let intP = P (fun s ->
        let intPart = s |> Seq.takeWhile Utils.isDigit |> System.String.Concat
        let rest = s.Substring(intPart.Length)
        if intPart = "" then None else Some(int64 intPart, rest))

    let surround pl pr p = pl >>? p <<? pr

    // Parses 0 or more of `p`, and returns the list of all parsed values
    let rec many (p: 'a Parser): 'a list Parser = P (fun s ->
        match parse p s with
        | Some (x, s') ->
            parse (many p) s' |> Option.map (fun (xs, s'') -> (x :: xs, s''))
        | None -> Some ([], s))

    let many1 p = (List.insertAt 0) <?> p <*>? many p

    let ws = many1 (charP ' ' <|>? charP '\t' <|>? charP '\n') >>? ret ()

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
    if s[n] <> '(' then
        failwith (sprintf "character under cursor must be `(`: `%s`, %i" s n)
    else
        let mutable depth = 1
        let mutable i = n + 1
        let finish = s.Length
        while i < finish && depth > 0 do
            if s[i] = '(' then depth <- depth + 1
            if s[i] = ')' then depth <- depth - 1
            i <- i + 1
        if s[i - 1] = ')' then
            (i - 1)
        else
            failwith (sprintf "couldn't find matching parenthesis: %s %d" s n)

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