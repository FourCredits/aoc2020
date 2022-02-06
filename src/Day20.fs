module Day20

open Utils.Parse

type Tile = Tile of (int64 * bool array array)

let parse lines =
    let tile (lines: string array): Tile =
        let idNumber =
            parse (surround (stringP "Tile ") (charP ':') intP) lines[0]
            |> Option.get
            |> fst
        let image =
            lines
            |> Array.tail
            |> Array.map (fun s -> s |> Seq.map ((=) '#') |> Seq.toArray)
        Tile (idNumber, image)
    lines |> Utils.split "" |> Seq.map (Seq.toArray >> tile)

// Note: only works on square images
let rotate (image: bool array array) =
    let size = Array.length image
    [| for i = 0 to size - 1 do
        [| for j = 0 to size - 1 do
            image[size - j - 1][i] |] |]

let flip (image: bool array array) =
    let size = Array.length image
    [| for i = 0 to size - 1 do
        [| for j = 0 to size - 1 do
            image[size - i - 1][j] |] |]

let rotations original =
    let r1 = rotate original
    let r2 = rotate r1
    let r3 = rotate r2
    [ original; r1; r2; r3 ]

let orientations (Tile (id, image)): Tile list =
    rotations image @ rotations (flip image)
    |> List.map (fun i -> Tile (id, i))

let leftEdge   (Tile (id, image)): bool array = Array.map Array.head image
let rightEdge  (Tile (id, image)): bool array = Array.map Array.last image
let topEdge    (Tile (id, image)): bool array = Array.head image
let bottomEdge (Tile (id, image)): bool array = Array.last image

let lineUpVertical   tile1 tile2 = rightEdge  tile1 = leftEdge tile2
let lineUpHorizontal tile1 tile2 = bottomEdge tile1 = topEdge  tile2

let lineUp tile1 tile2 =
    let lineUp' (tile1, tile2) =
        lineUpVertical tile1 tile2 || lineUpVertical tile2 tile1 ||
        lineUpHorizontal tile1 tile2 || lineUpHorizontal tile2 tile1
    (orientations tile1, orientations tile2)
    ||> List.allPairs
    |> List.exists lineUp'

let part1 (tiles: Tile seq) =
    [ for (Tile (id, _)) as t1 in tiles do
        Seq.except [t1] tiles
        |> Seq.filter (fun t2 -> lineUp t1 t2)
        |> Seq.length
        |> (fun n -> (id, n))]
    |> List.filter (snd >> ((=) 2))
    |> List.map fst
    |> List.reduce (*)

let corners (grid: 'a array array): 'a array =
    let yMax = Array.length grid
    let xMax = Array.length grid[0]
    [| grid[0][0]; grid[0][xMax]; grid[yMax][0]; grid[yMax][xMax] |]

let example = Utils.exampleify "Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###..."

let main lines =
    let parsed = parse lines
    printfn $"{part1 parsed}"
