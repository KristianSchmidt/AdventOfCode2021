#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 25
    |> Array.map (fun s -> s.ToCharArray() |> Array.map string)
    |> Helpers.toGridMap

let ex = """v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>"""

let dataEx =
    ex.Split('\n')
    |> Array.map (fun s -> s.ToCharArray() |> Array.map string)
    |> Helpers.toGridMap

let gridSizey = data |> Map.filter (fun (x,_) _ -> x = 0) |> Map.count
let gridSizex = data |> Map.filter (fun (_,y) _ -> y = 0) |> Map.count

let doMove (x,y) (state : Map<int*int,string>) =
    //printfn "first lookup %A" (x,y)
    let nextCoord =
        match state[(x,y)] with
        | "." -> None
        | ">" -> Some ((x+1) % gridSizex,y)
        | "v" -> Some (x, (y+1) % gridSizey)

    match nextCoord with
    | None -> None
    | Some c ->
        //printfn "second lookup %A" c
        match state[c] with
        | "." -> Some ((x,y),c)
        | _ -> None

let printState (m : Map<int*int,string>) =
    for y in 0..gridSizey-1 do
        for x in 0..gridSizex-1 do
            printf "%s" m[(x,y)]
        printfn ""
    printfn ""

let solve initState =
    let rec f state i =
        //printState state
        let newMovesEast =
            state
            |> Map.filter (fun k v -> v = ">")
            |> Map.toArray
            |> Array.choose (fun (k,v) -> doMove k state)

        let stateAfterEast =
            newMovesEast
            |> Array.fold (fun s (cOld,cNew) -> s |> Map.add cNew s[cOld] |> Map.add cOld ".") state

        let newMovesSouth =
            stateAfterEast
            |> Map.filter (fun k v -> v = "v")
            |> Map.toArray
            |> Array.choose (fun (k,v) -> doMove k stateAfterEast)

        let stateAfterSouth =
            newMovesSouth
            |> Array.fold (fun s (cOld,cNew) -> s |> Map.add cNew s[cOld] |> Map.add cOld ".") stateAfterEast

        if (newMovesEast.Length = 0 && newMovesSouth.Length = 0) then
            i
        else
            f stateAfterSouth (i+1)

    f initState 1


let ans1 = solve data

ans1

/// Part 2

let ans2 = data

ans2