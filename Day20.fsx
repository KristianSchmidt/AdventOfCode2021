#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let (enhance, input) =
    let data = Helpers.Web.getInput 20
    data[0].ToCharArray(), data[2..]

String.Join("", enhance)

let data =
    input
    |> Array.map (fun s -> s.ToCharArray() |> Array.map string)
    |> Helpers.toGridMap

let neighbors i map (x,y) =
    [|(x-1,y-1);(x,y-1);(x+1,y-1)
      (x-1,y);(x,y);(x+1,y)
      (x-1,y+1);(x,y+1);(x+1,y+1)
    |]
    |> Array.map (fun k -> Map.tryFind k map)
    |> Array.map (
        if (i % 2 = 0) then Option.defaultValue "." else Option.defaultValue "#")

let nToInt (arr : string array) =
    arr
    |> Array.map (function | "." -> "0" | "#" -> "1")
    |> (fun a -> String.Join("", a))
    |> (fun s -> Convert.ToInt32(s, 2))

let enhanceImg i (map : Map<int*int,string>) =
    let minX = map.Keys |> Seq.minBy fst |> fst
    let maxX = map.Keys |> Seq.maxBy fst |> fst
    let minY = map.Keys |> Seq.minBy snd |> snd
    let maxY = map.Keys |> Seq.maxBy snd |> snd

    seq {
        for x in (minX - 1) .. (maxX+1) do
            for y in (minY - 1) .. (maxY+1) do
                yield ((x,y),neighbors i map (x,y) |> nToInt |> Array.get enhance |> string)
    }
    |> Array.ofSeq
    |> Map.ofArray

let solve map iters =
    let rec f i data =
        if (i = iters) then
            data |> Map.toArray |> Array.countBy snd
        else
            f (i+1) (enhanceImg i data)

    f 0 map

let ans1 = solve data 2

ans1

/// Part 2

let ans2 = solve data 50

ans2