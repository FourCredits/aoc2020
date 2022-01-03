module Program

[<EntryPoint>]
let main args = 
    let day = int args[0]
    let ((f, file):(string seq -> unit) * string) =
        match day with
            | 1 -> (Day1.main, "resources/1.txt")
            | 2 -> (Day2.main, "resources/2.txt")
            | 3 -> (Day3.main, "resources/3.txt")
            | 4 -> (Day4.main, "resources/4.txt")
            | 5 -> (Day5.main, "resources/5.txt")
            | _ -> failwith "day not implemented yet"
    let input = System.IO.File.ReadLines file
    f input
    0
