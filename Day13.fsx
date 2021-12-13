#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 13

let firstEmpty = data |> Array.findIndex ((=)"")

let data1 =
    data[..firstEmpty-1]
    |> Array.map (Helpers.split ",")
    |> Array.map (fun [|x;y|] -> (int x, int y))

let folds =
    data[firstEmpty+1..]
    |> Array.map (fun s -> s.Replace("fold along ", ""))
    |> Array.map (Helpers.split "=")
    |> Array.map (fun [|e1;e2|] -> e1, int e2)

let fold (axis,foldPoint) (data : (int*int) array) =
    let (filterFunc, mapFunc) =
        match axis with
        | "x" -> (fun c -> (fst c) > foldPoint), (fun (x,y) -> foldPoint-(x-foldPoint),y)
        | "y" -> (fun c -> (snd c) > foldPoint), (fun (x,y) -> x,foldPoint-(y-foldPoint))
    
    let newPoints =
        data
        |> Array.filter filterFunc
        |> Array.map mapFunc
    let oldPoints =
        data |> Array.filter (filterFunc >> not)

    Array.concat [|oldPoints;newPoints|]
    |> Array.distinct

let ans1 =
    fold folds[0] data1
    |> Array.distinct
    |> Array.length

ans1

/// Part 2

let processed =
    folds
    |> Array.fold (fun s t -> fold t s) data1

let maxy = Array.maxBy snd processed |> snd
let maxx = Array.maxBy fst processed |> fst

for y in 0 .. maxy do
    let xs =
        [|0..maxx|]
        |> Array.map (fun x -> processed
                               |> Array.tryFind ((=)(x,y))
                               |> Option.map (fun _ -> '#')
                               |> Option.defaultValue '.')
    let s = String(xs)
    printfn "%s" s

//RKHFZGUB

let ans2 = data

ans2