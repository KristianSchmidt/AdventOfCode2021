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

data
|> Array.fold switchOnOff Map.empty
|> Map.filter (fun k v -> v = "on")
|> Map.count

let ans1 = data

ans1

/// Part 2

let data2 =
    data
    |> Array.map (fun (st,xmin, xmax, ymin, ymax, zmin, zmax) -> (st, int64 xmin, int64 xmax, int64 ymin, int64 ymax, int64 zmin, int64 zmax))

let axisOverlap (a1min,a1max,a2min,a2max) =
    if (a2min <= a1min && a1max <= a2max) then
        Some (a2min,a2max)
    else if (a1min <= a2min && a2max <= a1max) then
        Some (a1min,a1max)
    else if (a2min <= a1max && a1min <= a2max) then
        Some (a2min, a1max)
    else if (a1min <= a2max && a2min <= a1max) then
        Some (a1min, a2max)
    else
        None
        
let prod (c : (int64*int64) option) =
    match c with
    | Some (amin,amax) -> (amax-amin)
    | None -> 0L

let isOverlap ((st1,xmin1, xmax1, ymin1, ymax1, zmin1, zmax1),(st2,xmin2, xmax2, ymin2, ymax2, zmin2, zmax2)) =
    let xOverlap = axisOverlap (xmin1, xmax1, xmin2, xmax2)
        //max (min (xmax1 - xmin2) (xmax2 - xmin1)) 0L
    let yOverlap = axisOverlap (ymin1, ymax1, ymin2, ymax2)
        //max (min (ymax1 - ymin2) (ymax2 - ymin1)) 0L
    let zOverlap = axisOverlap (zmin1, zmax1, zmin2, zmax2)
        //max (min (zmax1 - zmin2) (zmax2 - zmin1)) 0L

    //printfn "x: %A. y: %A. z: %A" xOverlap yOverlap zOverlap

    let overlap = (prod xOverlap) * (prod yOverlap) * (prod zOverlap)
    if (overlap > 0L) then
        let (oxmin,oxmax) = if ((xmax1 - xmin2) > (xmax2 - xmin1)) then (xmin1, xmax2) else (xmin2, xmax1)
        let (oymin,oymax) = if ((ymax1 - ymin2) > (ymax2 - ymin1)) then (ymin1, ymax2) else (ymin2, ymax1)
        let (ozmin,ozmax) = if ((zmax1 - zmin2) > (zmax2 - zmin1)) then (zmin1, zmax2) else (zmin2, zmax1)
        Some (oxmin,oxmax,oymin,oymax,ozmin,ozmax)
    else
        None


isOverlap (("", 0L,1L,0L,1L,1L,2L),("", 0L,1L,0L,1L,0L,4L))

Array.allPairs data2 data2
|> Array.filter (fun (p1,p2) -> p1 <> p2)
|> Array.map (fun (p1,p2) -> (p1,p2),isOverlap (p1,p2))
|> Array.filter (snd >> Option.isSome)
|> Array.countBy (fst >> fst)
|> Array.sortByDescending snd

let ans2 = data

ans2

//
//         xmin1    xmax1
//     xmin2    xmax2
//
//
//       xmin1    xmax1
//          xmin2    xmax2
(*
((("on", -30L, 14L, -7L, 40L, -48L, 6L),
  ("off", 10L, 22L, 22L, 39L, -30L, -19L)),
 Some (10L, 14L, 22L, 40L, -48L, -19L));





*)