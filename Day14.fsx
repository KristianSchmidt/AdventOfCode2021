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
        | Some s ->
            [|s1;s|]
        | None -> [|s1|]
        )
    |> (fun arr' -> String.Join("",arr') + arr[arr.Length - 1])

let counts =
    [|1..10|]
    |> Array.fold (fun s i -> doIter rules s) poly
    |> (fun s -> s.ToCharArray())
    |> Array.countBy id

(counts |> Array.maxBy snd |> snd) - (counts |> Array.minBy snd |> snd)

let template = "NNCB"
let testRules1 = """CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C"""

let testRules =
    testRules1
    |> Helpers.split "\n"
    |> Array.map (Helpers.split " -> ")
    |> Array.map (fun [|pattern;rep|] -> (pattern,rep))
    |> Map.ofArray
    
[|1..1|]
|> Array.fold (fun s i -> doIter testRules s) template


//let ans1 = data

//ans1

/// Part 2

testRules



let startingMap =
    poly.ToCharArray()
    |> Array.windowed 2
    |> Array.map (fun arr -> String.Join("",arr))
    |> Array.countBy id
    |> Array.map (fun (k,v) -> k,int64 v)
    |> Map.ofArray

let testMap =
    template.ToCharArray()
    |> Array.windowed 2
    |> Array.map (fun arr -> String.Join("",arr))
    |> Array.map (fun s -> (s,1L))
    |> Map.ofArray

let evolve (rules : Map<string,string>) ((currMap : Map<string,int64>), endPattern) =
    let makeChanges (pattern,count) =
        match Map.tryFind pattern rules with
        | Some rep ->
            let c = string pattern[0]
            let c' = string pattern[1]
            [|(pattern,-count); ($"{c}{rep}", count); ($"{rep}{c'}",count) |]
        | None -> failwithf "."

    let allChanges =
        currMap
        |> Map.toArray
        |> Array.collect makeChanges

    let newEndpattern =
        Map.tryFind endPattern rules
        |> Option.map (fun rep -> $"{rep}{endPattern[1]}")
        |> Option.defaultValue endPattern
    
    let newMap =
        allChanges
        |> Array.fold
            (fun map (p,c) ->
                match Map.tryFind p map with
                | Some c' -> map |> Map.add p (c' + c)
                | None -> map |> Map.add p c) currMap

    newMap, newEndpattern


let counts2 =
    [|1..40|]
    |> Array.fold (fun s _ -> evolve rules s) (startingMap, poly[poly.Length-2..])

let counts3 =
    counts2
    |> fst
    |> Map.toArray
    |> Array.map (fun (s,c) -> (s[0],c))
    |> Array.groupBy fst
    |> Array.map (fun (c,arr) -> c, arr |> Array.sumBy snd)

counts3 |> Array.minBy snd

(counts3 |> Array.maxBy snd |> snd) + 1L - (counts3 |> Array.minBy snd |> snd)


let testCounts =
    [|1..40|]
    |> Array.fold (fun s _ -> evolve testRules s) (testMap, "CB")
    |> fst
    |> Map.toArray
    |> Array.map (fun (s,c) -> (s[0],c))
    |> Array.groupBy fst
    |> Array.map (fun (c,arr) -> c, arr |> Array.sumBy snd)


(testCounts |> Array.maxBy snd |> snd) + 1L - (testCounts |> Array.minBy snd |> snd)


// 2914365137499
// 2610798035608 too low
// 2188189693528 too low

snd counts2

poly[poly.Length-2..]

//evolve startingMap testRules "CB"

//let ans2 = data

//ans2