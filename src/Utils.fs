module Utils

// Turning a string into a sequence of lines
// -----------------------------------------

let exampleify (str: string) = str.Split "\n"

// Sketching out various things
// ----------------------------

let undefined () = failwith "undefined"

// Tuples
// ------

let first f (a, b) = (f a, b)

let second f (a, b) = (a, f b)

// Character predicates
// --------------------

let isDigit c = 48 <= int c && int c <= 57

let isHexDigit c = isDigit c || Seq.contains c "abcdef"

// Dealing with lists
// ------------------

let rec split x xs =
    if Seq.isEmpty xs then
        Seq.empty
    else
        let head = Seq.takeWhile ((<>) x) xs
        let tail = Seq.skipWhile ((<>) x) xs
        if Seq.isEmpty tail then
            seq { head }
        else
            seq { yield head; yield! (split x (Seq.tail tail)) }

// Iterating
// ---------

let rec iterate f x = seq { yield x; yield! (iterate f (f x))}

let converge step =
    iterate step
    >> Seq.pairwise
    >> Seq.skipWhile (fun (x, y) -> x <> y)
    >> Seq.head
    >> snd

// Parsing
// ------

module Parse =
    type 'a Parser = P of (string -> ('a * string) option)

    let parse (P p) s = p s

    let (<?>) f (P p) = P (fun s -> p s |> Option.map (first f))

    let ret a = P (fun s -> Some (a, s))

    let fail = P (fun _ -> None)

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
        let intPart = s |> Seq.takeWhile isDigit |> System.String.Concat
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

    let exactly (P p) = P (fun s ->
        match p s with
        | Some (a, "") -> Some (a, "")
        | _ -> None)

    let sepBy (p: 'a Parser) (sep: 'b Parser): 'a list Parser =
        (fun x xs -> x :: xs) <?> p <*>? many (sep >>? p)

    let anyOf (ps : 'a Parser seq): 'a Parser = ps |> Seq.reduce (<|>?)

    // A version of `many`, which counts how many of `p` it parses
    let rec countMany (p: 'a Parser): (int * 'a list) Parser = P (fun s ->
        match parse p s with
        | Some (x, s') ->
            parse (countMany p) s'
            |> Option.map (fun ((n, xs), s'') -> (((1 + n), x :: xs), s''))
        | None -> Some ((0, []), s))
