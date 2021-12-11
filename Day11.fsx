#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 11
    |> Array.map (fun s -> s.ToCharArray() |> Array.map (string >> int))
    |> Helpers.toGridMap

let adj (x,y) =
    [| (x-1, y+1); (x,y+1); (x+1, y+1);
       (x-1, y); (x+1, y)
       (x-1, y-1); (x,y-1); (x+1,y-1)|]
    |> Array.choose (fun c -> data |> Map.tryFind c |> Option.map (fun v -> c,v))

let solve data limit =
    let rec f state step flashes =
        let inc1 =
            state
            |> Map.map (fun k v -> v + 1)

        let rec f2 state doneFlashes queueFlashes =
            let newFlashes =
                state
                |> Map.filter (fun k v -> v > 9)
                |> Map.toArray
                |> Array.map fst
                |> Set.ofArray
                |> (fun s -> (s - doneFlashes) - queueFlashes)
            let newQueue =
                newFlashes + queueFlashes
            if (newQueue.IsEmpty) then
                let zeroed =
                    doneFlashes
                    |> Set.toArray
                    |> Array.fold (fun s n -> s |> Map.add n 0) state
                zeroed, doneFlashes.Count
            else
                let elem = newQueue.MaximumElement
                let neighbors = adj elem |> Array.map fst
                let newState =
                    neighbors
                    |> Array.fold (fun s n -> s |> Map.add n (s[n] + 1)) state
                let newDone = Set.add elem doneFlashes
                let newQueue2 = Set.remove elem newQueue
                f2 newState newDone newQueue2

        let newState, doneFlashes = f2 inc1 Set.empty Set.empty

        let newDoneFlashes = flashes + doneFlashes

        if (step = limit) then
            newDoneFlashes
        else
            f newState (step+1) newDoneFlashes


    f data 1 0

let ans1 = solve data 100

ans1

/// Part 2

let solve2 data =
    let rec f state step flashes =
        let inc1 =
            state
            |> Map.map (fun k v -> v + 1)

        let rec f2 state doneFlashes queueFlashes =
            let newFlashes =
                state
                |> Map.filter (fun k v -> v > 9)
                |> Map.toArray
                |> Array.map fst
                |> Set.ofArray
                |> (fun s -> (s - doneFlashes) - queueFlashes)
            let newQueue =
                newFlashes + queueFlashes
            if (newQueue.IsEmpty) then
                let zeroed =
                    doneFlashes
                    |> Set.toArray
                    |> Array.fold (fun s n -> s |> Map.add n 0) state
                zeroed, doneFlashes.Count
            else
                let elem = newQueue.MaximumElement
                let neighbors = adj elem |> Array.map fst
                let newState =
                    neighbors
                    |> Array.fold (fun s n -> s |> Map.add n (s[n] + 1)) state
                let newDone = Set.add elem doneFlashes
                let newQueue2 = Set.remove elem newQueue
                f2 newState newDone newQueue2

        let newState, doneFlashes = f2 inc1 Set.empty Set.empty

        let newDoneFlashes = flashes + doneFlashes

        if (doneFlashes = 100) then
            step
        else
            f newState (step+1) newDoneFlashes

    f data 1 0

let ans2 = solve2 data

ans2