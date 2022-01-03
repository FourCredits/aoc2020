module Day2

let input = "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc"

type PasswordEntry =
    { N1   : int
      N2   : int
      C    : char
      Pass : string }

let parse (line:string) =
    let words = line.Split()
    let range = words[0].Split("-")
    { N1 = int range[0]; N2 = int range[1]; C = words[1][0]; Pass = words[2] }

let valid1 (p: PasswordEntry) =
    let count =
        Seq.toList p.Pass
        |> List.filter (fun x -> x = p.C)
        |> List.length
    p.N1 <= count && count <= p.N2

let valid2 p =
    let v1 = (p.Pass[p.N1 - 1] = p.C)
    let v2 = (p.Pass[p.N2 - 1] = p.C)
    (v1 && not v2) || (not v1 && v2)

let solve pred (passwords: PasswordEntry list) =
    passwords
    |> Seq.filter pred
    |> Seq.length

let part1 = solve valid1
let part2 = solve valid2

let main lines =
    let parsed = lines |> Seq.map parse |> Seq.toList
    printfn $"{part1 parsed}, {part2 parsed}"
