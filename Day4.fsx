#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 4

let numbers = Helpers.split "," data[0] |> Array.map int |> List.ofArray

type Num =
    | Marked of int
    | Unmarked of int

let isMarked = function | Marked _ -> true | _ -> false

let makeBoard (a : string array array) =
    a
    |> Array.mapi (fun r arr -> arr |> Array.mapi (fun c s -> (r,c),Unmarked (int s)))
    |> Array.collect id
    |> Map.ofArray

let boards =
    data[2..]
    |> Array.map (fun s -> s.Split(" ", StringSplitOptions.RemoveEmptyEntries))
    |> Array.filter (fun a -> a <> Array.empty)
    |> Array.chunkBySize 5
    |> Array.map makeBoard

let checkRow  (board : Map<(int*int),Num>) (r : int) =
    [|0..4|]
    |> Array.map (fun c -> Map.find (r,c) board)
    |> Array.forall isMarked

let checkCol (board : Map<(int*int),Num>) (c : int) =
    [|0..4|]
    |> Array.map (fun r -> Map.find (r,c) board)
    |> Array.forall isMarked

let checkBoard (board : Map<(int*int),Num>) =
    let isRowComplete = [|0..4|] |> Array.exists (checkRow board)
    let isColComplete = [|0..4|] |> Array.exists (checkCol board)
    
    isRowComplete || isColComplete

let updateBoard n (board : Map<(int*int),Num>) =
    board
    |> Map.toArray
    |> Array.map (fun ((r,c),num) ->
        let newNum = match num with | Unmarked x when x = n -> Marked x | someNum -> someNum
        (r,c),newNum
        )
    |> Map.ofArray

let (lastNum,winner) =
    let rec f (boards : Map<(int*int),Num> array) nums =
        match nums with
        | n :: xs ->
            printfn "Processing %i" n
            let newBoards =
                boards
                |> Array.map (updateBoard n)
            match Array.tryFind checkBoard newBoards with
            | Some b ->
                (n,b)
            | None ->
                f newBoards xs
        | _ -> failwithf "done"

    f boards numbers

let ans1 =
    let unmarkedSum =
        winner
        |> Map.toArray
        |> Array.sumBy (fun (_,n) -> match n with | Unmarked x -> x | _ -> 0)
    unmarkedSum * lastNum

ans1

/// Part 2

let (lastNumLoser,loser) =
    let rec f (boards : Map<(int*int),Num> array) nums =
        match nums with
        | n :: xs ->
            printfn "Processing %i" n
            printfn "Boards length: %i" (Array.length boards)
            let newBoards =
                boards
                |> Array.map (updateBoard n)
            let losers = newBoards |> Array.filter (fun b -> checkBoard b |> not)
            match losers with
            | [| |] -> n,Array.head newBoards
            | _ -> f losers xs
        | _ ->
            failwithf "done"

    f boards numbers    

let ans2 =
    let unmarkedSum =
        loser
        |> Map.toArray
        |> Array.sumBy (fun (_,n) -> match n with | Unmarked x -> x | _ -> 0)
    unmarkedSum*lastNumLoser
    
ans2