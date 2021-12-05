#load "Helpers.fsx"

open System
open System.Text.RegularExpressions

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let data =
    Helpers.Web.getInput 5
    |> Array.map (function | Regex "(\d+),(\d+) -> (\d+),(\d+)" [d1;d2;d3;d4] -> (int d1,int d2),(int d3,int d4))

let makeSegment ((x1,y1),(x2,y2)) =
    seq {
        for x in x1 .. x2 do
            for y in y1 .. y2 do
                yield (x,y)
    } |> Array.ofSeq

let ans1 =
    data
    |> Array.filter (fun ((x1,y1),(x2,y2)) -> x1 = x2 || y1 = y2)
    |> Array.map (fun (c1,c2) -> Array.sort [|c1;c2|])
    |> Array.collect (fun [|c1;c2|] -> makeSegment (c1,c2))
    |> Array.countBy id
    |> Array.filter (snd >> (fun i -> i > 1))
    |> Array.length

ans1

/// Part 2

let makeSegment2 ((x1,y1),(x2,y2)) =
    let diff1 = x2-x1
    let diff2 = y2-y1
    if ((abs diff1) <> (abs diff2)) then
        failwithf "."
    let diff = abs diff1
    seq {
        for i in 0 .. diff do
            yield (x1+(i*Math.Sign(diff1)), y1+(i*Math.Sign(diff2)))
    } |> Seq.toArray

let data1 =
    data
    |> Array.filter (fun ((x1,y1),(x2,y2)) -> x1 = x2 || y1 = y2)
    |> Array.map (fun (c1,c2) -> Array.sort [|c1;c2|])
    |> Array.collect (fun [|c1;c2|] -> makeSegment (c1,c2))
    
let data2 =
    data
    |> Array.filter (fun ((x1,y1),(x2,y2)) -> not (x1 = x2 || y1 = y2))
    |> Array.map (fun (c1,c2) -> Array.sort [|c1;c2|])
    |> Array.collect (fun [|c1;c2|] -> makeSegment2 (c1,c2))

let ans2 =
    [|data1;data2|]
    |> Array.collect id
    |> Array.countBy id
    |> Array.filter (snd >> (fun i -> i > 1))
    |> Array.length

ans2