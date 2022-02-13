module Day25

open System.Collections.Generic

let memoize f =
    let d = Dictionary()
    fun a ->
        if d.ContainsKey a then
            d[a]
        else
            let value = f a
            d.Add(a, value)
            value

let numLoops =
    let inner (publicKey: int64): int =
        Utils.iterate (fun i -> (i * 7L) % 20201227L) 1L
        |> Seq.findIndex ((=) publicKey)
    memoize inner

let calculatePublicKey subjectNumber loopSize =
    Utils.iterateN (fun i -> (i * subjectNumber) % 20201227L) loopSize 1L

let part1 k1 k2 =
    let loop2 = numLoops k2
    calculatePublicKey k1 loop2

let main lines =
    let k1, k2 =
        match Array.map (int: string -> int) lines with
        | [| k1; k2 |] -> k1, k2
        | _ -> failwith "parse error"
    part1 k1 k2
    |> printfn "%d"
