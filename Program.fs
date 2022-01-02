module Program

let (mains:(string seq -> unit) list) = [ Day1.main; Day2.main ]

[<EntryPoint>]
let main args = 
    let day = int args[0]
    let input = System.IO.File.ReadAllLines $"resources/{day}.txt"
    let f = mains[day - 1]
    f input
    0
