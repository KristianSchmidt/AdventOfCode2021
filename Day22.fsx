#load "Helpers.fsx"

open System
open System.Text.RegularExpressions

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let data =
    Helpers.Web.getInput 22
    |> Array.map (function | Regex "(\w+) x=(.+)\.\.(.+),y=(.+)\.\.(.+),z=(.+)\.\.(.+)" [st;xmin;xmax;ymin;ymax;zmin;zmax] -> (st,int xmin, int xmax, int ymin, int ymax, int zmin, int zmax))


let switchOnOff m (st,xmin, xmax, ymin, ymax, zmin, zmax) =
    let mutable map = m
    for i in (max -50 xmin)..(min 50 xmax) do
        for j in (max -50 ymin)..(min 50 ymax) do
            for k in (max -50 zmin)..(min 50 zmax) do
                    map <- Map.add (i,j,k) st map

    map

let ans1 =
    data
    |> Array.fold switchOnOff Map.empty
    |> Map.filter (fun k v -> v = "on")
    |> Map.count

ans1

/// Part 2

let data2 =
    data
    |> Array.map (fun (st,xmin, xmax, ymin, ymax, zmin, zmax) -> (st, int64 xmin, int64 xmax, int64 ymin, int64 ymax, int64 zmin, int64 zmax))

let axisOverlap (a1min,a1max,a2min,a2max) =
    //printfn "%A" (a1min,a1max,a2min,a2max)
    if (a2min > a1max || a1min > a2max) then
        None
    else
        Some (max a1min a2min, min a1max a2max)
    
let isOverlap ((st1,xmin1, xmax1, ymin1, ymax1, zmin1, zmax1),(st2,xmin2, xmax2, ymin2, ymax2, zmin2, zmax2)) =
    let xOverlap = axisOverlap (xmin1, xmax1, xmin2, xmax2)
    let yOverlap = axisOverlap (ymin1, ymax1, ymin2, ymax2)
    let zOverlap = axisOverlap (zmin1, zmax1, zmin2, zmax2)

    //printfn "x: %A. y: %A. z: %A" xOverlap yOverlap zOverlap

    match xOverlap, yOverlap, zOverlap with
    | Some (xomin,xomax), Some (yomin,yomax), Some (zomin,zomax) ->
        let st = match st1,st2 with
                 | "on","on" -> "off"
                 | "off", "on" -> "on"
                 | "off","off" -> "on"
                 | "on", "off" -> "off"
        
        Some (st,xomin,xomax,yomin,yomax,zomin,zomax)
    | _ -> None

let area (_,xmin,xmax,ymin,ymax,zmin,zmax) =
    ((xmax-xmin) + 1L) *
    ((ymax-ymin) + 1L) *
    ((zmax-zmin) + 1L)

let getSt (st,_,_,_,_,_,_) = st

let solve data =
    let rec f processed queue =
        match queue with
        | [] -> processed
        | x :: xs ->
            let overlaps =
                processed
                |> List.choose (fun p -> isOverlap (p, x))
            //printfn "x: %A" x
            //printfn "Overlaps: %A" overlaps
            let newProcessed =
                if (getSt x = "off") then
                    List.concat [overlaps;processed]
                else
                    List.concat [[x];overlaps;processed]
            f newProcessed xs

    f [] (List.ofArray data)

let ans2 =
    let m =
        solve data2
        |> List.map (fun c -> c, area c)
        |> List.map (fun ((st,_,_,_,_,_,_),a) -> st, a)
        |> List.groupBy fst 
        |> List.map (fun (k,v) -> k, v |> List.sumBy snd)
        |> Map.ofList
    m["on"] - m["off"]

ans2