#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 6
    |> Array.head
    |> Helpers.split ","
    |> Array.map int

let data1 = 
    data
    |> Array.countBy id
    |> Array.map (fun (k,v) -> (k,int64 v))
    |> Map.ofArray

let simulate map until =
    let rec f (map : Map<int,int64>) i =
        //printfn "Map %A" map
        let newMap =
            map
            |> Map.toArray
            |> Array.collect (fun (k,v) -> match k with
                                           | 0 -> [|(6,v); (8,v)|]
                                           | _ -> [|(k-1,v)|])
            |> Array.groupBy fst
            |> Array.map (fun (k,v) -> k, Array.sumBy snd v)
            |> Map.ofArray
        let length = newMap |> Map.toArray |> Array.sumBy snd
        if (i = until) then
            length
        else
            //printfn "Length: %i" length 
            f newMap (i+1)

    f map 1

let ans1 = simulate data1 80

ans1

/// Part 2

let ans2 = simulate data1 256

ans2