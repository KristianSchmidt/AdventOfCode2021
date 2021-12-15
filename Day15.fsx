#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 15
    |> Array.map (fun s -> s.ToCharArray() |> Array.map (string >> int))
    |> Helpers.toGridMap

let maxX = 99
let maxY = 99

let adj (x,y) =
    [|(x-1,y); (x+1,y); (x,y-1); (x,y+1)|]
    |> Array.filter (fun (x,y) -> x >= 0 && y >= 0 && x <= maxX && y <= maxY)

let dists =
    data
    |> Map.toArray
    |> Array.collect
        (fun (k, v) ->
            adj k
            |> Array.map (fun k' -> (k,k'),data[k']))
    |> Map.ofArray
    
let x = Helpers.Dijkstra.dijkstra2 dists (0,0)

let ans1 = x[(99,99)]

ans1

/// Part 2

let incData i =
    data |> Map.map (fun k v -> if (v+1 = 10) then 1 else v+1)

let incKeys (x,y,(map : Map<int*int,int>)) =
    map 
    |> Map.toArray
    |> Array.map (fun ((x',y'),v) -> (x' + x*(maxX + 1), y' + y*(maxY + 1)), v)

let bigmap =
    seq {
        for x in 0..4 do
            for y in 0..4 do
                yield (x,y,incData (x+y))
    
    }
    |> Seq.toArray
    |> Array.map incKeys
    |> Array.map (Map.ofArray)
    |> Array.fold Helpers.Map.merge Map.empty

let adj2 (x,y) =
    [|(x-1,y); (x+1,y); (x,y-1); (x,y+1)|]
    |> Array.filter (fun (x,y) -> x >= 0 && y >= 0 && x <= (500-1) && y <= (500-1))

let dists2 =
    bigmap
    |> Map.toArray
    |> Array.collect
        (fun (k, v) ->
            adj2 k
            |> Array.map (fun k' -> (k,k'),bigmap[k']))
    |> Map.ofArray

let bigDists = Helpers.Dijkstra.dijkstra2 dists2 (0,0)


let ans2 = data

ans2