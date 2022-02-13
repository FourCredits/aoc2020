module Day23

open System.Collections.Generic

(*
 *  Big takeaways from this one:
 *
 *  - Don't be afraid of big numbers. I saw the numbers 1000000 and 10000000,
 *    and figured that the problem was to find a loop, or do some memoization.
 *    Turns out, it's easier to just go through all 10 million iterations of a
 *    1 million size list, so long as you do it correctly
 *  - Linked lists are really useful for these times when you are rearranging a
 *    structure while keeping basically the same order. The solutions I came up
 *    with had a whole lot of unnecessary copying. While I could get away with
 *    that when the list was 9 elements long, you absolutely can't do that when
 *    the list is a million times.
 *  - Dictionaries are a good way of creating a linked list when you want to do
 *    the base manipulations of a list (which you don't really have access to
 *    on the in-built List).
 *)

let prepareLinkedList (cups: int array) (fullLength: int) =
    let numCups = Array.length cups
    [0 .. fullLength - 1]
    |> Seq.map (fun i ->
        if i < numCups - 1 then
            (cups[i], cups[i + 1])
        elif i = numCups - 1 && numCups = fullLength then
            (cups[i], cups[0])
        elif i = numCups - 1 && numCups < fullLength then
            (cups[i], Array.max cups + 1)
        elif i = fullLength - 1 then
            (i + 1, cups[0])
        else
            (i + 1, i + 2)
        |> KeyValuePair)
    |> Dictionary

let solveGame (cups: int array) (fullLength: int) (numMoves: int) =
    let cuplist = prepareLinkedList cups fullLength
    let wrap n =
        let rec f n d = if n < 0 then f (n + d) d else n % d
        (f (n - 2) fullLength) + 1
    let mutable ptr = cups[0]
    for _ = 1 to numMoves do
        // remove three cups
        let c1 = cuplist[ptr]
        let c2 = cuplist[c1]
        let c3 = cuplist[c2]
        cuplist[ptr] <- cuplist[c3]
        // find dest
        let dest =
            Utils.iterate wrap (wrap ptr)
            |> Seq.find (fun i -> i <> c1 && i <> c2 && i <> c3)
        // reinsert cups after dest
        cuplist[c3] <- cuplist[dest]
        cuplist[dest] <- c1
        // advance ptr
        ptr <- cuplist[ptr]
    cuplist

let part1 cups =
    let solved = solveGame cups (Array.length cups) 100
    Utils.iterate (fun x -> solved[x]) solved[1]
    |> Seq.takeWhile ((<>) 1)
    |> Seq.fold (fun acc i -> (acc * 10) + i) 0

let part2 cups =
    let finalCups = solveGame cups 1000000 10000000
    (int64 finalCups[1]) * (int64 finalCups[finalCups[1]])

let main lines =
    let parsed =
        lines
        |> Array.exactlyOne
        |> Seq.map (fun (c: char) -> int c - 48)
        |> Array.ofSeq
    printfn $"{part1 parsed}, {part2 parsed}"
