#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 3
    |> Array.map (fun s -> s.ToCharArray())

let getCommon (a : char array) =
    a
    |> Array.countBy id
    |> Array.maxBy snd
    |> fst

let getUnCommon (a : char array) =
    a
    |> Array.countBy id
    |> Array.minBy snd
    |> fst

let gamma =
    [|0..11|]
    |> Array.map (fun i -> data |> Array.map (fun a -> a[i]))
    |> Array.map getCommon
    |> (fun c -> String(c))

let epsilon =
    [|0..11|]
    |> Array.map (fun i -> data |> Array.map (fun a -> a[i]))
    |> Array.map getUnCommon
    |> (fun c -> String(c))
    
let ans1 = data

2601*1494

ans1

/// Part 2

let getCommonNum (a : char array array) (idx : int) =
    let t = a
            |> Array.map (fun a' -> a'[idx])
            |> Array.countBy id
    if ((t |> Array.map snd |> Array.distinct |> Array.length) = 1) then
        '1'
    else
        t
        |> Array.maxBy snd
        |> fst

let getUnCommonNum (a : char array array) (idx : int) =
    let t = a
            |> Array.map (fun a' -> a'[idx])
            |> Array.countBy id
    if ((t |> Array.map snd |> Array.distinct |> Array.length) = 1) then
        '0'
    else
        t
        |> Array.minBy snd
        |> fst

let oxygen =
    let rec f idx (elems : char array array) =
        if (elems.Length = 1) then
            elems[0]
        else
            let common = getCommonNum elems idx
            let newElems = elems |> Array.filter (fun a -> a[idx] = common)
            f (idx+1) newElems

    String(f 0 data)

let co2 =
    let rec f idx (elems : char array array) =
        if (elems.Length = 1) then
            elems[0]
        else
            let uncommon = getUnCommonNum elems idx
            let newElems = elems |> Array.filter (fun a -> a[idx] = uncommon)
            f (idx+1) newElems

    String(f 0 data)

let ans2 = 3775*1159

ans2