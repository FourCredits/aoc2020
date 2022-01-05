module Day12

type Direction =
    | North
    | East
    | South
    | West

type Position = (int * int)

let parse (line:string) = (line[0], int line[1..])

let manhattan (y, x) = abs y + abs x

module Part1 =
    type State = { direction : Direction; position : Position }

    let turnLeft = function
        | North -> West
        | West  -> South
        | South -> East
        | East  -> North

    let turnRight = function
        | North -> East
        | East  -> South
        | South -> West
        | West  -> North

    let turnLeftN start = function
        | 90  -> start |> turnLeft
        | 180 -> start |> turnLeft |> turnLeft
        | 270 -> start |> turnLeft |> turnLeft |> turnLeft
        | 360 -> start
        | _   -> failwith "can't do that, sorry"

    let turnRightN start n = turnLeftN start (360 - n)

    let rec executeInstruction state (c, n) =
        match c with
            | 'N' ->
                let (y, x) = state.position
                { state with position = (y + n, x) }
            | 'S' ->
                let (y, x) = state.position
                { state with position = (y - n, x) }
            | 'E' ->
                let (y, x) = state.position
                { state with position = (y, x + n) }
            | 'W' ->
                let (y, x) = state.position
                { state with position = (y, x - n) }
            | 'L' ->
                { state with direction = turnLeftN state.direction n }
            | 'R' ->
                { state with direction = turnRightN state.direction n }
            | 'F' ->
                let direction =
                    match state.direction with
                        | North -> 'N' | South -> 'S' | West  -> 'W' | East  -> 'E'
                executeInstruction state (direction, n)
            | _ -> failwith "unrecognized instruction"

    let part1 instructions =
        let start  = { direction = East; position = (0, 0) }
        let finish = Seq.fold executeInstruction start instructions
        manhattan finish.position

module Part2 =
    type State = { position : Position; waypoint : Position }

    let turnLeft (wayY, wayX) = function
        | 90  -> (wayX, -wayY)
        | 180 -> (-wayY, -wayX)
        | 270 -> (-wayX, wayY)
        | 360 -> (wayY, wayX)
        | _   -> failwith "can't turn that amount"

    let turnRight pos n = turnLeft pos (360 - n)

    let executeInstruction state (c, n) =
        match c with
            | 'N' ->
                let (y, x) = state.waypoint
                { state with waypoint = (y + n, x) }
            | 'S' ->
                let (y, x) = state.waypoint
                { state with waypoint = (y - n, x) }
            | 'E' ->
                let (y, x) = state.waypoint
                { state with waypoint = (y, x + n) }
            | 'W' ->
                let (y, x) = state.waypoint
                { state with waypoint = (y, x - n) }
            | 'L' ->
                { state with waypoint = turnLeft state.waypoint n }
            | 'R' ->
                { state with waypoint = turnRight state.waypoint n }
            | 'F' ->
                let (posY, posX) = state.position
                let (wayY, wayX) = state.waypoint
                { state with position = (posY + n * wayY, posX + n * wayX) }
            | _   -> failwith "unrecognized instruction"

    let part2 instructions =
        let start  = { position = (0, 0); waypoint = (1, 10) }
        let finish = Seq.fold executeInstruction start instructions
        manhattan finish.position

let example =
    let str = "F10
N3
F7
R90
F11"
    str.Split "\n" |> Seq.toList

let main lines =
    let parsed = lines |> Seq.map parse
    (* printfn $"{Part2.part2 parsed}" *)
    printfn $"{Part1.part1 parsed}, {Part2.part2 parsed}"
