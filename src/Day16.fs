module Day16

// TODO: extract 'range' concepts

type Ticket = Ticket of int array
type Range = Range of (int * int)
type Field = Field of string * Range * Range

let parseRange (s: string) =
    match s.Split "-" with
    | [| n1; n2 |] -> Range (int n1, int n2)
    | _ -> failwith "unexpected format"

let evalRange (Range (n1, n2)) = set { n1 .. n2 }

let inRange (Range (n1, n2)) n = n1 <= n && n <= n2

let parse (lines: string seq) =
    match lines |> Utils.split "" |> Seq.map Seq.toList |> Seq.toList with
    | [fields; ["your ticket:"; yourTicket]; otherTickets] ->
        let parseTicket (s: string) = Ticket (s.Split "," |> Array.map int)
        let parseField (s:string): Field =
            let fieldName = (s.Split ":")[0]
            let rest = ((s.Split ":")[1]).Split " "
            let r1 = parseRange (rest[1])
            let r2 = parseRange (rest[3])
            Field (fieldName, r1, r2)
        let fields' = fields |> Seq.map parseField
        let (yourTicket') = parseTicket yourTicket
        let otherTickets' = otherTickets |> Seq.tail |> Seq.map parseTicket
        (fields', yourTicket', otherTickets')
    | _ -> failwith "unexpected format"

let validRange (fields: Field seq): int Set =
    let addToRange range (Field (_, r1, r2)) =
        Set.unionMany [range; evalRange r1; evalRange r2]
    fields |> Seq.fold addToRange Set.empty

let outOfRange range (Ticket fields) =
    match fields |> Array.filter (fun x -> not (Set.contains x range)) with
    | [| |] -> None
    | xs -> Some xs

let part1 (fields, myTicket, otherTickets) =
    let range = validRange fields
    otherTickets |> Seq.choose (outOfRange range) |> Seq.concat |> Seq.sum

let classify (fields: Field seq) (tickets: Ticket seq): Map<int, string> =
    let couldBe nums (Field (_, r1, r2)) =
        nums |> Seq.forall (fun n -> inRange r1 n || inRange r2 n)
    let mutable associations = Map.empty
    let mutable remaining = Set.ofSeq fields
    while not (Set.isEmpty remaining) do
        let (i, (Field (fieldName, _, _) as field)) =
            Seq.except (Map.keys associations) [0 .. Seq.length fields - 1]
            |> Seq.pick (fun i ->
                let ns = Seq.map (fun (Ticket fields) -> fields[i]) tickets
                match remaining |> Seq.filter (couldBe ns) |> Seq.toList with
                | [field] -> Some (i, field)
                | _ -> None)
        associations <- Map.add i fieldName associations
        remaining <- Set.remove field remaining
    associations

let part2 (fields, (Ticket myTicket), otherTickets) =
    let range = validRange fields
    let validTickets =
        otherTickets |> Seq.filter ((outOfRange range) >> Option.isNone)
    classify fields validTickets
    |> Map.filter (fun _ (v: string) -> v.StartsWith "departure")
    |> Map.toSeq
    |> Seq.map (fun (k, _) -> int64 myTicket[k])
    |> Seq.reduce (*)

let main lines =
    let parsed = parse lines
    printfn $"{part1 parsed}, {part2 parsed}"
