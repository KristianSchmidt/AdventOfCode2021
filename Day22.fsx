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

let isOverlap ((st1,xmin1, xmax1, ymin1, ymax1, zmin1, zmax1),(st2,xmin2, xmax2, ymin2, ymax2, zmin2, zmax2)) =
    let xOverlap =
        max (min (xmax1 - xmin2) (xmax2 - xmin1)) 0L
    let yOverlap =
        max (min (ymax1 - ymin2) (ymax2 - ymin1)) 0L
    let zOverlap =
        max (min (zmax1 - zmin2) (zmax2 - zmin1)) 0L

    xOverlap * yOverlap * zOverlap

Array.allPairs data2 data2
|> Array.filter (fun (p1,p2) -> p1 <> p2)
|> Array.map (fun (p1,p2) -> (p1,p2),isOverlap (p1,p2))
|> Array.filter (snd >> (<>)0L)
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
((("on", -4L, 40L, -40L, 11L, -35L, 9L),
  ("on", -44L, 8L, -24L, 26L, -13L, 31L)), 9240L);
((("on", -4L, 40L, -40L, 11L, -35L, 9L),
  ("off", -2L, 15L, -35L, -23L, -4L, 10L)), 4199L);


*)