module Day11

open Utils

let example =
    let str = "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"
    str.Split "\n" |> Seq.toList

type Seat =
    | Empty
    | Floor
    | Filled

let parse lines =
    let posParse = function
        | 'L' -> Empty
        | '.' -> Floor
        | '#' -> Filled
        | _   -> failwith "unrecognized character"
    Seq.toArray lines
    |> Array.map (Seq.toArray >> Array.map posParse)

let inBounds xs (y, x) =
    Array.length xs > y && y >= 0 && Array.length xs[0] > x && x >= 0

let step (rules:int -> Seat -> Seat) neighbours seats =
    [| for i in [0 .. Array.length seats - 1] do
        [| for j in [0 .. Array.length seats[0] - 1] do
            let adjacents =
                neighbours seats (i, j)
                |> Seq.filter ((=) Filled)
                |> Seq.length
            rules adjacents (seats[i][j]) |] |]

let getScore =
    Array.map (Array.filter ((=) Filled) >> Array.length) >> Array.sum

let neighbours1 (seats:'a array array) (y, x) =
    [ for i in [y - 1 .. y + 1] do
        for j in [x - 1 .. x + 1] do
            if inBounds seats (i, j) then seats[i][j] ]

let rules1 adjacents = function
    | Floor  -> Floor
    | Empty  -> if adjacents = 0 then Filled else Empty
    | Filled -> if adjacents > 4 then Empty else Filled

let part1 = converge (step rules1 neighbours1) >> getScore

let neighbours2 seats (y, x) =
    let directions =
        [(-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1)]
    directions
    |> Seq.choose (fun (dY, dX) ->
        iterate (fun (y', x') -> (y' + dY, x' + dX)) (y + dY, x + dX)
        |> Seq.takeWhile (inBounds seats)
        |> Seq.map (fun (y', x') -> seats[y'][x'])
        |> Seq.tryFind ((<>) Floor))

let rules2 adjacents = function
    | Floor  -> Floor
    | Empty  -> if adjacents = 0 then Filled else Empty
    | Filled -> if adjacents >= 5 then Empty else Filled

let part2 = converge (step rules2 neighbours2) >> getScore

let main lines =
    let parsed = parse lines
    printfn $"{part1 parsed}, {part2 parsed}"
