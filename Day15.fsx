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
    data
    |> Map.map (fun k v ->
        match v+i with
        | x when x >= 10 -> x - 9
        | x -> x)

let incKeys (x,y) (map : Map<int*int,int>) =
    map 
    |> Map.toArray
    |> Array.map (fun ((x',y'),v) -> (x' + x*(maxX + 1), y' + y*(maxY + 1)), v)
    |> Map.ofArray

let bigmap =
    seq {
        for x in 0..4 do
            for y in 0..4 do
                yield incData (x+y) |> incKeys (x,y)
    }
    |> Seq.toArray
    |> Array.fold Helpers.Map.merge Map.empty

open System.Collections.Generic

type CostFunc = (int*int) -> (int*int) -> int

let astar start goal (adj : (int*int) -> (int*int) array) (cost : CostFunc) (heuristic : CostFunc) =
    let frontier = PriorityQueue<int*int,int>()
    frontier.Enqueue(start, 0)
    let cameFrom = Dictionary<int*int,int*int>()
    let costSoFar = Dictionary<int*int,int>()
    cameFrom[start] <- (-1,-1)
    costSoFar[start] <- 0
    let mutable isDone = false
    let mutable processed = 0

    while (frontier.Count > 0 && isDone = false) do
        let current = frontier.Dequeue()
        processed <- processed + 1

        if (current = goal) then
            isDone <- true
        else
            for next in adj current do
                let newCost = costSoFar[current] + (cost current next)
                if (not (costSoFar.ContainsKey(next)) || newCost < costSoFar[next]) then
                    costSoFar[next] <- newCost
                    let priority = newCost + (heuristic next goal)
                    frontier.Enqueue(next, priority)
                    cameFrom[next] <- current

    printfn "Processed %i nodes" processed
    cameFrom

let adj2 (x,y) =
    [|(x-1,y); (x+1,y); (x,y-1); (x,y+1)|]
    |> Array.filter (fun (x,y) -> x >= 0 && y >= 0 && x <= (500-1) && y <= (500-1))

#time "on"

let path =
    astar (0,0) (499,499)
          adj2
          (fun p1 p2 -> bigmap[p2])
          (fun (x1,y1) (x2,y2) -> 5*(abs (x2-x1) + abs (y2-y1)))

let traverse start goal (path : Map<int*int,int*int>) =
    let rec f pathTaken =
        match pathTaken with
        | x :: xs when x = goal -> xs // Don't include the start
        | x :: xs -> f (path[x] :: pathTaken)

    f [start] |> List.toArray

let ans2 =
    traverse (499,499) (0,0) (path |> Seq.toArray |> Array.map (fun kv -> kv.Key, kv.Value) |> Map.ofArray)
    |> Array.sumBy (fun k -> bigmap[k])

ans2