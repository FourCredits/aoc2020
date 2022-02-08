module Day23

open System.Diagnostics

let rec findDestinationCup cups currentCup =
    // match Array.tryFindIndex ((=) (currentCup - 1)) cups with
    // | Some x -> x
    // | None ->
    //     let currentCup' = currentCup - 1
    //     if Array.forall ((<) currentCup') cups then
    //         Array.findIndex ((=) (Array.max cups)) cups
    //     else
    //         findDestinationIndex cups currentCup'
    [(currentCup - 1) .. -1 .. 1]
    |> Seq.filter (fun cup -> Array.contains cup cups)
    |> Seq.tryHead
    |> Option.defaultValue (Array.max cups)

let solve iterations startingCups =
    let mutable cups = startingCups
    let mutable currentCup = Array.head startingCups
    for i = 1 to iterations do
        let currentIndex = Array.findIndex ((=) currentCup) cups
        let threeCups =
            [| 1 .. 3 |]
            |> Array.map (fun i -> cups[(currentIndex + i) % (Array.length startingCups)])
        cups <- Array.filter (fun cup -> not (Array.contains cup threeCups)) cups
        let destinationCup = findDestinationCup cups currentCup
        let destinationIndex = Array.findIndex ((=) destinationCup) cups
        cups <- Array.insertManyAt (destinationIndex + 1) threeCups cups
        let currentIndex = Array.findIndex ((=) currentCup) cups
        currentCup <- cups[(currentIndex + 1) % Array.length cups]
    cups

let part1 startingCups =
    let finalCups = solve 100 startingCups
    finalCups
    |> Array.splitAt (Array.findIndex ((=) 1) finalCups)
    |> fun (a, b) -> Array.append (Array.tail b) a
    |> Array.map string
    |> String.concat ""
    |> int

let main lines =
    let parsed =
        lines
        // [| "389125467" |]
        |> Array.exactlyOne
        |> Seq.map (fun (c: char) -> int c - 48)
        |> Array.ofSeq
    printfn $"{part1 parsed}"
