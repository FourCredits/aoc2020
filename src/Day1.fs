module Day1

let example = [1721; 979; 366; 299; 675; 1456]

let reals = System.IO.File.ReadAllLines("resources/1.txt") |> Seq.map int

let part1 nums =
    [ for a in nums do
        for b in nums do
            yield (a, b) ]
    |> Seq.filter (fun (a, b) -> a + b = 2020 && a < b)
    |> Seq.map (fun (a, b) -> a * b)
    |> Seq.max

let part2 nums =
    [ for a in nums do
        for b in nums do
            for c in nums do
                yield(a, b, c) ]
    |> Seq.filter (fun (a, b, c) -> a + b + c = 2020 && a < b && a < c)
    |> Seq.map (fun (a, b, c) -> a * b * c)
    |> Seq.max

let main (lines:string seq) =
    let nums = Seq.map int lines
    printfn $"{part1 nums}, {part2 nums}"
