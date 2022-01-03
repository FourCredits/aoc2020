module Utils

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

