#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let dataTest = """start-A
start-b
A-c
A-b
b-d
A-end
b-end"""

let data =
    Helpers.Web.getInput 12
    |> Array.map (Helpers.split "-")
    |> Array.collect (fun [|e1;e2|] -> [|(e1,e2);(e2,e1)|])
    |> Array.groupBy fst
    |> Array.map (fun (e,arr) -> e,arr |> Array.map snd)
    |> Map.ofArray

let testInput =
    dataTest
    |> Helpers.split "\n"
    |> Array.map (Helpers.split "-")
    |> Array.collect (fun [|e1;e2|] -> [|(e1,e2);(e2,e1)|])
    |> Array.groupBy fst
    |> Array.map (fun (e,arr) -> e,arr |> Array.map snd)
    |> Map.ofArray

let isUpper (s : string) = s.ToUpper() = s

let solve data =
    let rec f currPath (paths : string list list) visited =
        let currNode = List.head currPath
        let availChoices =
            data
            |> Map.tryFind currNode
            |> Option.map (fun d -> (Set.ofArray d) - visited)
            |> Option.defaultValue Set.empty
        printfn "Curr: %A. Avail: %A" currPath availChoices
        if (currNode = "end") then
            currPath :: paths
        else if (Set.isEmpty availChoices) then
            paths
        else
            availChoices
            |> Set.toList
            |> List.collect (fun c ->
                if (isUpper c) then
                    f (c :: currPath) paths visited
                else
                    f (c :: currPath) paths (Set.add c visited)
                )

    f ["start"] List.empty (Set.ofArray [|"start"|])

solve testInput
|> List.length

let ans1 =
    solve data
    |> List.distinct
    |> List.length

ans1

/// Part 2

let solve2 data =
    let rec f currPath (paths : string list list) visited smallVisited =
        let currNode = List.head currPath
        let availChoices =
            data
            |> Map.tryFind currNode
            |> Option.map (fun d -> (Set.ofArray d) - visited)
            |> Option.defaultValue Set.empty
        //printfn "Curr: %A. Avail: %A" currPath availChoices
        if (currNode = "end") then
            currPath :: paths
        else if (Set.isEmpty availChoices) then
            paths
        else
            availChoices
            |> Set.toList
            |> List.collect (fun c ->
                if (isUpper c) then
                    f (c :: currPath) paths visited smallVisited
                else
                    match smallVisited with
                    | Some v when v = c ->
                        f (c :: currPath) paths (Set.add c visited) smallVisited
                    | Some v when v <> c ->
                        f (c :: currPath) paths (Set.add c visited) smallVisited
                    | None ->
                        let chooseAsLower =
                            f (c :: currPath) paths visited (Some c)
                        let dontChooseAsLower =
                            f (c :: currPath) paths (Set.add c visited) smallVisited
                        List.concat [chooseAsLower; dontChooseAsLower]
                )

    f ["start"] List.empty (Set.ofArray [|"start"|]) None

solve2 testInput
|> List.map (List.rev)
|> List.distinct
|> List.map (fun lst -> String.Join('-', lst))
|> List.iter (printfn "%s")

let ans2 =
    solve2 data
    |> List.distinct
    |> List.length

ans2