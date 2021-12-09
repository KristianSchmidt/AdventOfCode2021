#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 9
    |> Array.map (fun s -> s.ToCharArray() |> Array.map (string >> int))
    |> Array.mapi (fun y a -> a |> Array.mapi (fun x e -> ((x,y),e)))
    |> Array.collect id
    |> Map.ofArray

let neighbors (x,y) =
    [|(x,y-1); (x,y+1);(x+1,y);(x-1,y)|]
    |> Array.choose (fun k -> Map.tryFind k data)

let ans1 =
    data
    |> Map.toArray
    |> Array.choose (fun (k,v) ->
        let m = Array.min (neighbors k)
        if (v < m) then
            Some (v+1)
        else None
        )
    |> Array.sum
    

ans1

/// Part 2

let lows =
    data
    |> Map.toArray
    |> Array.choose (fun (k,v) ->
        let m = Array.min (neighbors k)
        if (v < m) then Some k else None
        )

let adj key =
    let neighbors (x,y) =
        [|(x,y-1); (x,y+1);(x+1,y);(x-1,y)|]
        |> Array.choose (fun k -> Map.tryFind k data |> Option.map (fun v -> k,v))

    neighbors key
    |> Array.filter (snd >> ((<>)9))
    |> Array.map fst

let size k =
    Helpers.BFS.bfs adj k |> Map.count

let ans2 =
    lows
    |> Array.map size
    |> Array.sortDescending
    |> Array.take 3
    |> Array.reduce (*)

ans2