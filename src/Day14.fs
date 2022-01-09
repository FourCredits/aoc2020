module Day14

type Instruction =
    | UpdateMask of string
    | WriteToMem of key: int64 * value: int64

type State =
    { mask   : int64 * int64
      memory : Map<int64,int64> }

let parse (line:string) =
    let words = line.Split " "
    if words[0] = "mask" then
        UpdateMask words[2]
    else
        let close = Seq.findIndex ((=) ']') line
        let key = int64 (line[4 .. close - 1])
        let value = int64 words[2]
        WriteToMem (key, value)

module Part1 =
    let applyMask mask n =
        let mask' = "0b" + mask
        let ons = int64 (String.map (fun c -> if c = 'X' then '0' else c) mask')
        let offs = int64 (String.map (fun c -> if c = 'X' then '1' else c) mask')
        (n ||| ons) &&& offs

    let part1 instructions =
        let mutable mask = ""
        let mutable memory = Map.empty
        for ins in instructions do
            match ins with
            | UpdateMask mask' -> mask <- mask'
            | WriteToMem (k, v) -> memory <- Map.add k (applyMask mask v) memory
        memory |> Map.values |> Seq.sum

module Part2 =
    let bits n =
        let rec bits' n =
            if n = 0L then
                seq { 0L }
            else
                seq { yield (n &&& 1L) ; yield! bits' (n >>> 1) }
        Seq.rev (bits' n)

    let leftPad n x xs = Seq.append (Seq.replicate (n - Seq.length xs) x) xs

    let applyMask mask k =
        let rec f ns maskBit keyBit: int64 seq =
            match maskBit with
            | '1' -> Seq.map (fun k -> (k <<< 1) ||| 1) ns
            | '0' -> Seq.map (fun k -> (k <<< 1) ||| keyBit) ns
            | 'X' -> Seq.append (f ns '0' 0) (f ns '0' 1)
            | _ -> failwith "unrecognized mask value"
        (* let bs = k |> bits |> leftPad 36 0 *)
        Seq.fold2 f [0L] mask (k |> bits |> leftPad 36 0L)

    let part2 instructions =
        let mutable mask = ""
        let mutable memory = Map.empty
        for ins in instructions do
            match ins with
            | UpdateMask mask' -> mask <- mask'
            | WriteToMem (k, v) ->
                for k' in applyMask mask k do
                    memory <- Map.add k' v memory
        memory |> Map.values |> Seq.sum

let main lines =
    let parsed = Seq.map parse lines
    printfn $"{Part1.part1 parsed}, {Part2.part2 parsed}"
