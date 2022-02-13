module Day24

open Utils.Parse

let directionParser =
    [ ("se", ( 1, -1))
      ("e",  ( 2,  0))
      ("sw", (-1, -1))
      ("w",  (-2,  0))
      ("nw", (-1,  1))
      ("ne", ( 1,  1)) ]
    |> Seq.map (fun (s, offset) -> stringP s >>? ret offset)
    |> anyOf
    |> many

let reduce directions =
    directions |> Seq.fold (fun (x1, y1) (x2, y2) -> (x1 + x2, y1 + y2)) (0, 0)

let part1 lines =
    lines
    |> Array.map (parse directionParser >> Option.get >> fst >> reduce)
    |> Array.fold
        (fun m k -> Map.change k (Option.defaultValue 0 >> ((+) 1) >> Some) m)
        Map.empty
    |> Map.filter (fun _ v -> v % 2 = 1)

type Tile = White | Black

let neighbours (a, b) =
    [(1, -1); (2, 0); (-1, -1); (-2, 0); (-1, 1); (1, 1)]
    |> List.map (fun (c, d) -> (a + c, b + d))

let gameOfTilesRules (numBlackNeighbours: int) (tile: Tile): Tile =
    match (numBlackNeighbours, tile) with
    | (n, Black) when n = 0 || n > 2 -> White
    | (2, White) -> Black
    | (_, t) -> t

let extrema positions =
    let (l, _) = Seq.minBy fst positions
    let (r, _) = Seq.maxBy fst positions
    let (_, t) = Seq.minBy snd positions
    let (_, b) = Seq.maxBy snd positions
    (l, t), (r, b)

let expand ((l, t), (r, b)) = ((l - 2, t - 1), (r + 2, b + 1))

let step (bounds, board) =
    let ((l, t), (r, b)) as bounds' = expand bounds
    let board' =
        Map [ for pos in Seq.allPairs [l .. r] [t .. b] ->
                let numBlackNeighbours =
                    neighbours pos
                    |> Seq.filter (fun k ->
                        Map.containsKey k board && board[k] = Black)
                    |> Seq.length
                let tile = Map.tryFind pos board |> Option.defaultValue White
                pos, gameOfTilesRules numBlackNeighbours tile ]
    (bounds', board')

let part2 start =
    let startingBoard = Map.map (fun k _ -> Black) start
    let bounds = extrema start.Keys
    Utils.iterateN step 100 (bounds, startingBoard)
    |> snd
    |> Map.filter (fun _ v -> v = Black)
    |> Map.count

let main lines =
    let part1 = part1 lines
    printfn $"{Map.count part1}, {part2 part1}"
