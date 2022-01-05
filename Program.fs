module Program

[<EntryPoint>]
let main args = 
    let day = int args[0]
    let (f, file) =
        match day with
            | 1 -> (Day1.main, "resources/1.txt")
            | 2 -> (Day2.main, "resources/2.txt")
            | 3 -> (Day3.main, "resources/3.txt")
            | 4 -> (Day4.main, "resources/4.txt")
            | 5 -> (Day5.main, "resources/5.txt")
            | 6 -> (Day6.main, "resources/6.txt")
            | 7 -> (Day7.main, "resources/7.txt")
            | 8 -> (Day8.main, "resources/8.txt")
            | 9 -> (Day9.main, "resources/9.txt")
            | 10 -> (Day10.main, "resources/10.txt")
            | 11 -> (Day11.main, "resources/11.txt")
            | 12 -> (Day12.main, "resources/12.txt")
            | _ -> failwith "day not implemented yet"
    let input = System.IO.File.ReadLines file
    f input
    0
