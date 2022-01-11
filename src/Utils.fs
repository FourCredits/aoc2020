module Utils

let isDigit c = 48 <= int c && int c <= 57

let isHexDigit c = isDigit c || Seq.contains c "abcdef"

let rec split x xs =
    if Seq.isEmpty xs then
        Seq.empty
    else
        let head = Seq.takeWhile ((<>) x) xs
        let tail = Seq.skipWhile ((<>) x) xs
        if Seq.isEmpty tail then
            seq { head }
        else
            seq { yield head; yield! (split x (Seq.tail tail)) }

let rec iterate f x = seq { yield x; yield! (iterate f (f x))}

let converge step =
    iterate step
    >> Seq.pairwise
    >> Seq.skipWhile (fun (x, y) -> x <> y)
    >> Seq.head
    >> snd

