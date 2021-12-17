#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 7
    |> Array.head
    |> Helpers.split ","
    |> Array.map int

let min = Array.min data
let max = Array.max data

let ans1 = 
    [|min .. max|]
    |> Array.map (fun i -> data |> Array.sumBy (fun j -> abs (i - j)))
    |> Array.min

ans1

/// Part 2

let cost i = i * (i+1) / 2

let ans2 =
    [|min .. max|]
    |> Array.map (fun i -> data |> Array.sumBy (fun j -> cost (abs (i - j))))
    |> Array.min

ans2