module Day17

let parse lines =
    lines
    |> Seq.toArray
    |> Array.map (Seq.toArray >> Array.map ((=) '#'))

let inRange (lo, hi) n = lo <= n && n <= hi

module Part1 =
    let neighbours (z, y, x) =
        [| for a = z - 1 to z + 1 do
            for b = y - 1 to y + 1 do
                for c = x - 1 to x + 1 do
                    if (z, y, x) <> (a, b, c) then
                        (a, b, c) |]

    let inRegion grid (z, y, x) =
        let zMax = Array.length grid
        let yMax = Array.length grid[0]
        let xMax = Array.length (grid[0][0])
        inRange (0, xMax - 1) x &&
        inRange (0, yMax - 1) y &&
        inRange (0, zMax - 1) z

    let extend n grid =
        let zMax = Array.length grid
        let yMax = Array.length grid[0]
        let xMax = Array.length (grid[0][0])
        [| for z = -n to zMax + n - 1 do
            [| for y = -n to yMax + n - 1 do
                [| for x = -n to xMax + n - 1 do
                    inRegion grid (z, y, x) && grid[z].[y].[x] |] |] |]

    let tick grid =
        let zMax = Array.length grid
        let yMax = Array.length grid[0]
        let xMax = Array.length (grid[0][0])
        [| for a = 0 to zMax - 1 do
            [| for b = 0 to yMax - 1 do
                [| for c = 0 to xMax - 1 do
                    let activeCubes =
                        neighbours (a, b, c)
                        |> Seq.filter (fun (z, y, x) ->
                            inRegion grid (z, y, x) && grid[z].[y].[x])
                        |> Seq.length
                    match (grid[a].[b].[c], activeCubes) with
                    | (false, 3) | (true, (2 | 3))-> true
                    | _ -> false |] |] |]

    let tickN n grid =
        grid
        |> extend n
        |> Utils.iterate tick
        |> Seq.item n

    let part1 plane =
        plane
        |> Array.singleton
        |> tickN 6
        |> Array.concat
        |> Array.concat
        |> Array.filter id
        |> Array.length

module Part2 =
    let neighbours (z, y, x, w) =
        [| for a = z - 1 to z + 1 do
            for b = y - 1 to y + 1 do
                for c = x - 1 to x + 1 do
                    for d = w - 1 to w + 1 do
                        if (z, y, x, w) <> (a, b, c, d) then
                            (a, b, c, d) |]

    let inRegion grid (z, y, x, w) =
        let zMax = Array.length grid
        let yMax = Array.length grid[0]
        let xMax = Array.length grid[0].[0]
        let wMax = Array.length grid[0].[0].[0]
        inRange (0, zMax - 1) z &&
        inRange (0, yMax - 1) y &&
        inRange (0, xMax - 1) x &&
        inRange (0, wMax - 1) w

    let extend n grid =
        let zMax = Array.length grid
        let yMax = Array.length grid[0]
        let xMax = Array.length grid[0].[0]
        let wMax = Array.length grid[0].[0].[0]
        [| for z = -n to zMax + n - 1 do
            [| for y = -n to yMax + n - 1 do
                [| for x = -n to xMax + n - 1 do
                    [| for w = -n to wMax + n - 1 do
                        inRegion grid (z, y, x, w) &&
                        grid[z].[y].[x].[w] |] |] |] |]

    let tick grid =
        let zMax = Array.length grid
        let yMax = Array.length grid[0]
        let xMax = Array.length grid[0].[0]
        let wMax = Array.length grid[0].[0].[0]
        [| for a = 0 to zMax - 1 do
            [| for b = 0 to yMax - 1 do
                [| for c = 0 to xMax - 1 do
                    [| for d = 0 to wMax - 1 do
                        let activeCubes =
                            neighbours (a, b, c, d)
                            |> Seq.filter (fun (z, y, x, w) ->
                                inRegion grid (z, y, x, w) &&
                                grid[z].[y].[x].[w])
                            |> Seq.length
                        match (grid[a].[b].[c].[d], activeCubes) with
                        | (false, 3) | (true, (2 | 3))-> true
                        | _ -> false |] |] |] |]

    let tickN n grid =
        grid
        |> extend n
        |> Utils.iterate tick
        |> Seq.item n

    let part2 plane =
        plane
        |> Array.singleton
        |> Array.singleton
        |> tickN 6
        |> Array.concat
        |> Array.concat
        |> Array.concat
        |> Array.filter id
        |> Array.length

let main lines =
    let parsed = parse lines
    printfn $"{Part1.part1 parsed}, {Part2.part2 parsed}"
