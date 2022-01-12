﻿module Program

let runDay (f, input) =
    let input = System.IO.File.ReadAllLines input
    f input

[<EntryPoint>]
let main args =
    let days = [|
        (Day1.main, "resources/1.txt")
        (Day2.main, "resources/2.txt")
        (Day3.main, "resources/3.txt")
        (Day4.main, "resources/4.txt")
        (Day5.main, "resources/5.txt")
        (Day6.main, "resources/6.txt")
        (Day7.main, "resources/7.txt")
        (Day8.main, "resources/8.txt")
        (Day9.main, "resources/9.txt")
        (Day10.main, "resources/10.txt")
        (Day11.main, "resources/11.txt")
        (Day12.main, "resources/12.txt")
        (Day13.main, "resources/13.txt")
        (Day14.main, "resources/14.txt")
        (Day15.main, "resources/15.txt")
        (Day16.main, "resources/16.txt")
        (Day17.main, "resources/17.txt")
        (Day18.main, "resources/18.txt")
    |]
    if args.[0] = "all" then days |> Array.iter runDay
    else
        let day = int args.[0]
        match Array.tryItem (day - 1) days with
        | Some pair -> runDay pair
        | None -> printfn "Day %d isn't implemented yet" day
    0
