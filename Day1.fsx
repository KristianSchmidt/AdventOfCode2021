#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 1

let ans1 =
    data
    |> Array.map int
    |> Array.windowed 2
    |> Array.filter (fun [|x1;x2|] -> x2 > x1)
    |> Array.length

ans1

/// Part 2

let ans2 = 
    data
    |> Array.map int
    |> Array.windowed 3
    |> Array.map Array.sum
    |> Array.windowed 2
    |> Array.filter (fun [|x1;x2|] -> x2 > x1)
    |> Array.length

ans2