module Day8

let example =
    let str = "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"
    str.Split "\n" |> Seq.toList

type Instruction =
    | Nop
    | Acc
    | Jmp

let parse (line:string) =
    let words = line.Split " "
    let arg = int words[1]
    (match words[0] with
        | "nop" -> Nop
        | "acc" -> Acc
        | "jmp" -> Jmp
        | _     -> failwith "unrecognized instruction"
    , arg)

let run instructions =
    let finish = Array.length instructions
    let rec run' visited pc acc =
        if pc = finish then
            Ok acc
        else if Set.contains pc visited then
            Error acc
        else
            let visited' = Set.add pc visited
            match instructions[pc] with
                | (Nop, _) -> run' visited' (pc + 1) acc
                | (Acc, n) -> run' visited' (pc + 1) (acc + n)
                | (Jmp, n) -> run' visited' (pc + n) acc
    run' Set.empty 0 0

let part1 instructions =
    match run instructions with
        | Ok _    -> failwith "Hmm. This shouldn't happen."
        | Error n -> n

let part2 instructions =
    Array.pick id
        [| for i = 0 to Array.length instructions - 1 do
            match instructions[i] with
                | (Acc, _) -> None
                | (Nop, n) ->
                    let instruction' = (Jmp, n)
                    match Array.updateAt i instruction' instructions |> run with
                        | Ok    n -> Some n
                        | Error _ -> None
                | (Jmp, n) ->
                    let instruction' = (Nop, n)
                    match Array.updateAt i instruction' instructions |> run with
                        | Ok    n -> Some n
                        | Error _ -> None |]

let main lines =
    let parsed = lines |> Seq.map parse |> Seq.toArray
    printfn $"{part1 parsed}, {part2 parsed}"
