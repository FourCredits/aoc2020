module Day18

let undefined () = failwith "undefined"

module Parse =

    type Parser<'a> =
        { parse: string -> Option<'a * string>; }

    let map f p =
        { parse = fun s ->
            match p.parse s with
            | Some(a, rest) -> Some(f a, rest)
            | None -> None }

    let ret a = { parse = fun s -> Some(a, s) }

    let join pf pa =
        { parse = fun s ->
            match pf.parse s with
            | None -> None
            | Some(f, rest) ->
                match pa.parse rest with
                | Some(a, rest') -> Some (f a, rest')
                | None -> None }

    let rec sequence (parsers: Parser<'a> list): Parser<'a list> =
        match parsers with
        | [] -> ret []
        | p :: ps ->
            join (map (fun v -> (fun vs -> v :: vs)) p) (sequence ps)

    let charP c =
        { parse = fun s ->
            match s |> Seq.toList with
            | c' :: rest when c' = c -> Some(c, s[1..])
            | _ -> None }

    let stringP (s: string): Parser<string> =
        s
        |> Seq.map charP
        |> Seq.toList
        |> sequence
        |> map (fun s -> System.String.Concat(s))

type Expr =
    | Plus of Expr * Expr
    | Mult of Expr * Expr
    | Literal of int

let rec eval (expr: Expr): int =
    match expr with
    | Plus (sub1, sub2) -> eval sub1 + eval sub2
    | Mult (sub1, sub2) -> eval sub1 * eval sub2
    | Literal n -> n

let main lines =
    ()
