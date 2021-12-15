#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let (poly,rules) =
    let d = Helpers.Web.getInput 14
    let poly = d[0]
    let replace =
        d[2..]
        |> Array.map (Helpers.split " -> ")
        |> Array.map (fun [|pattern;rep|] -> (pattern,rep))
        |> Map.ofArray
    (poly,replace)

let doIter rules (poly : string) =
    let arr = poly.ToCharArray() |> Array.map string
    arr
    |> Array.windowed 2
    |> Array.collect (fun [|s1;s2|] ->
        match Map.tryFind $"{s1}{s2}" rules with
        | Some s -> [|s1;s|]
        | None   -> [|s1|]
        )
    |> (fun arr' -> String.Join("",arr') + arr[arr.Length - 1])

let counts =
    [|1..10|]
    |> Array.fold (fun s i -> doIter rules s) poly
    |> (fun s -> s.ToCharArray())
    |> Array.countBy id

let ans1 = (counts |> Array.maxBy snd |> snd) - (counts |> Array.minBy snd |> snd)

ans1

/// Part 2

let startingMap =
    poly.ToCharArray()
    |> Array.windowed 2
    |> Array.map (fun arr -> String.Join("",arr))
    |> Array.countBy id
    |> Array.map (fun (k,v) -> k,int64 v)
    |> Map.ofArray

let evolve (rules : Map<string,string>) (currMap : Map<string,int64>) =
    let makeChanges (pattern,count) =
        let rep = rules[pattern]
        [| (pattern,             -count)
           ($"{pattern[0]}{rep}", count)
           ($"{rep}{pattern[1]}", count) |]

    currMap
    |> Map.toArray
    |> Array.collect makeChanges
    |> Array.fold
        (fun map (p,c) ->
            match Map.tryFind p map with
            | Some c' -> map |> Map.add p (c + c')
            | None    -> map |> Map.add p c) currMap

let getEndPattern (rules : Map<string,string>) start iters =
    let rec f curr iters =
        match iters with
        | 0 -> curr
        | x -> f $"{rules[curr][0]}{curr[1]}" (x-1)

    f start iters

let endChar =
    (getEndPattern rules poly[poly.Length-2..] 40)[1]
    |> (fun s -> (s,1L))

let counts2 =
    [|1..40|]
    |> Array.fold (fun s _ -> evolve rules s) startingMap
    |> Map.toArray
    |> Array.map (fun (s,c) -> (s[0],c))
    |> Array.append [|endChar|]
    |> Array.groupBy fst
    |> Array.map (fun (c,arr) -> c, arr |> Array.sumBy snd)

let ans2 =
    let max = counts2 |> Array.maxBy snd |> snd
    let min = counts2 |> Array.minBy snd |> snd
    max - min

ans2