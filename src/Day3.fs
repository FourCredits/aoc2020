module Day3

open Utils

let example =
    let str = "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#"
    str.Split("\n") |> Seq.toList

let parse lines =
    lines
    |> Seq.map (Seq.toList >> List.map (fun x -> x = '#'))
    |> Seq.toList

let treesHit map (dy, dx) =
    let finish = List.length map
    let wrap = List.length map[0]
    iterate (fun (y, x) -> (y + dy, (x + dx) % wrap)) (0, 0)
        |> Seq.takeWhile (fun (y, _) -> y < finish)
        |> Seq.toList
        |> Seq.filter (fun (y, x) -> map[y][x])
        |> Seq.length

let part1 map = treesHit map (1, 3)
let part2 map =
    [(1, 1); (1, 3); (1, 5); (1, 7); (2, 1)]
    |> List.map (treesHit map >> uint64)
    |> List.reduce (*)

let main lines =
    let map = parse lines
    printfn $"{part1 map}, {part2 map}"
