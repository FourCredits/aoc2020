module Day21

open Utils.Parse
open System.Collections.Generic

let parse (line: string): (string list * string list) =
    let ingredientsP: string list Parser = sepBy wordP ws
    let allergensP: string list Parser =
        surround
            (stringP " (contains ")
            (charP ')')
            (sepBy wordP (stringP ", "))
    let p: (string list * string list) Parser =
        (fun a b -> (a, b)) <?> ingredientsP <*>? allergensP
    parse p line |> Option.get |> fst

let example = Utils.exampleify "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)"

let constructIngredientTable lines =
    let allAllergens: string Set = lines |> Seq.collect snd |> Set.ofSeq
    let allIngredients: string Set = lines |> Seq.collect fst |> Set.ofSeq
    (allIngredients, (Seq.replicate (Set.count allIngredients) allAllergens))
    ||> Seq.zip
    |> Map.ofSeq

let narrowIngredientTable lines (ingredientTable: Map<string, string Set>) =
    let table = ingredientTable :> IDictionary<_,_> |> Dictionary
    for (ingredients, allergens) in lines do
        let allergens = Set.ofList allergens
        for KeyValue (k, v) in table do
            if not (List.contains k ingredients) then
                table[k] <- Set.difference v allergens
    table |> Seq.map (|KeyValue|) |> Map.ofSeq

let findInertIngredients ingredientTable =
    ingredientTable
    |> Map.filter (fun _ v -> Set.isEmpty v)
    |> Map.keys
    |> Set.ofSeq

let findAllergenAssociations ingredientTable inertIngredients =
    let table = ingredientTable :> IDictionary<_,_> |> Dictionary
    let associatedAllergens = Dictionary()
    let numInerts = Set.count inertIngredients
    while associatedAllergens.Count < table.Count - numInerts do
        for KeyValue (ingredient, allergens) in table do
            if Set.count allergens = 1 then
                let allergen = Set.minElement allergens
                associatedAllergens.Add(ingredient, allergen)
                for KeyValue (k, allergens) in table do
                    table[k] <- Set.remove allergen allergens
    associatedAllergens |> Seq.map (|KeyValue|) |> Map.ofSeq

let part1 lines inertIngredients =
    lines
    |> Seq.collect fst
    |> Seq.filter (fun ingredient -> Set.contains ingredient inertIngredients)
    |> Seq.length

let part2 associatedAllergens =
    associatedAllergens
    |> Seq.sortBy (fun (KeyValue (_, v)) -> v)
    |> Seq.map (fun (KeyValue (k, _)) -> k)
    |> String.concat ","

let solve (lines: (string list * string list) array) =
    let table =
        lines |> constructIngredientTable |> narrowIngredientTable lines
    let inertIngredients = findInertIngredients table
    let associatedAllergens = findAllergenAssociations table inertIngredients
    printfn "%d, %s" (part1 lines inertIngredients) (part2 associatedAllergens)

let main lines =
    let parsed = Array.map parse lines
    solve parsed