module Day4

open Utils

let example =
    let str = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in"
    str.Split("\n") |> Seq.toList

let parse (lines:string seq) =
    let parseLine (line:string) =
        line.Split " "
        |> Seq.map ((fun x -> x.Split ":") >> (fun arr -> (arr[0], arr[1])))
    lines
    |> split ""
    |> Seq.map (String.concat " " >> parseLine)

let valid1 pass =
    let fields = Seq.map fst pass
    let requiredFields = ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"]
    Seq.forall (fun field -> Seq.contains field fields) requiredFields

let lookup pass field =
    pass
    |> Seq.filter (fst >> (=) field)
    |> Seq.head
    |> snd

let validHgt field =
    let num = String.filter System.Char.IsDigit field |> int
    let u = String.filter (fun c -> not (System.Char.IsDigit c)) field
    match u with
       | "cm" -> 150 <= num && num <= 193
       | "in" -> 59 <= num && num <= 76
       | _    -> false

let valid2 pass =
    let pass' =
        pass
        |> Seq.filter (fun (field, _) -> field <> "cid")
        |> Seq.sort
        |> Seq.toList
    match pass' with
        | [("byr", byr); ("ecl", ecl); ("eyr", eyr); ("hcl", hcl)
           ("hgt", hgt); ("iyr", iyr); ("pid", pid)] ->
           Seq.reduce (&&) [
            String.length pid = 9 && String.forall System.Char.IsDigit pid
            Seq.contains ecl ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]
            hcl[0] = '#' && String.forall Utils.isHexDigit (hcl[1..])
            2020 <= int eyr && int eyr <= 2030
            2010 <= int iyr && int iyr <= 2020
            1920 <= int byr && int byr <= 2002
            validHgt hgt ]
        | _ -> false

let solve pred passports =
    passports
    |> Seq.filter pred
    |> Seq.length

let part1 = solve valid1
let part2 = solve valid2

let main lines =
    let parsed = parse lines
    printfn $"{part1 parsed}, {part2 parsed}"
