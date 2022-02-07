module Day22

let parse lines =
    let player lines =
        lines |> Seq.tail |> Seq.map int |> Array.ofSeq

    match lines |> Utils.split "" |> List.ofSeq with
    | [ a; b ] -> (player a, player b)
    | _ -> failwith "too many players!"

let uncons array = Array.head array, Array.tail array

let calculateScore winningDeck =
    winningDeck
    |> Seq.rev
    |> Seq.zip (Seq.initInfinite ((+) 1))
    |> Seq.map (fun (a, b) -> a * b)
    |> Seq.sum

let playCombatRound (deck1, deck2) =
    let top1, rest1 = uncons deck1
    let top2, rest2 = uncons deck2

    if top1 > top2 then
        (Array.append rest1 [| top1; top2 |], rest2)
    else
        (rest1, Array.append rest2 [| top2; top1 |])

let playCombat startingDeck1 startingDeck2 =
    (startingDeck1, startingDeck2)
    |> Utils.iterate playCombatRound
    |> Seq.skipWhile (fun (a, b) -> not (Array.isEmpty a || Array.isEmpty b))
    |> Seq.head
    |> fun (a, b) -> if Array.isEmpty a then b else a
    |> calculateScore

type Winner =
    | Player1
    | Player2

let rec playRecursiveCombat startingDeck1 startingDeck2 : (Winner * int array) =
    let mutable deck1 = startingDeck1
    let mutable deck2 = startingDeck2
    let mutable seen = Set.empty
    let mutable stop = false

    while not stop do
        seen <- Set.add (deck1, deck2) seen
        let newDeck1, newDeck2 = playRecursiveCombatRound deck1 deck2
        deck1 <- newDeck1
        deck2 <- newDeck2

        stop <-
            Array.isEmpty deck1
            || Array.isEmpty deck2
            || Set.contains (deck1, deck2) seen

    if Set.contains (deck1, deck2) seen
       || Array.isEmpty deck2 then
        (Player1, deck1)
    elif Array.isEmpty deck1 then
        (Player2, deck2)
    else
        failwith "Unreachable...?"

and playRecursiveCombatRound deck1 deck2 =
    let (card1, deck1') = uncons deck1
    let (card2, deck2') = uncons deck2

    if card1 > Array.length deck1'
       || card2 > Array.length deck2' then
        // We can't recur, therefore determine round winner via highest card
        if card1 > card2 then
            (Array.append deck1' [| card1; card2 |], deck2')
        else
            (deck1', Array.append deck2' [| card2; card1 |])
    else
        let subDeck1 = Array.take card1 deck1'
        let subDeck2 = Array.take card2 deck2'

        if fst (playRecursiveCombat subDeck1 subDeck2) = Player1 then
            (Array.append deck1' [| card1; card2 |], deck2')
        else
            (deck1', Array.append deck2' [| card2; card1 |])

let main lines =
    let deck1, deck2 = parse lines

    printfn
        "%d, %d"
        (playCombat deck1 deck2)
        (playRecursiveCombat deck1 deck2
         |> snd
         |> calculateScore)
