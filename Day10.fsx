#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 10
    |> Array.map (fun s -> s.ToCharArray() |> Array.map string)

let isOpen = function | "(" | "{" | "[" | "<" -> true | _ -> false
let isClose = isOpen >> not

let isCorrectClose openChar closeChar =
    match openChar, closeChar with
    | "(", ")"
    | "{", "}"
    | "<", ">"
    | "[", "]" -> true
    | _ -> false

let getCorrupt (arr : string array) =
    let rec f opens lst =
        //printfn "Next: %A. LastOpen: %A" (List.tryHead lst) (List.tryHead opens)
        match lst with
        | x :: xs when isOpen x -> f (x :: opens) xs
        | x :: xs when isClose x ->
            if (isCorrectClose (List.head opens) x) then
                f (List.tail opens) xs
            else
                match x with
                | ")" -> 3 | "]" -> 57 | "}" -> 1197 | ">" -> 25137
        | [] -> 0

    f [] (List.ofArray arr)

let ans1 =
    data
    |> Array.map getCorrupt
    |> Array.sum

ans1

/// Part 2

let incomplete =
    data |> Array.map (fun arr -> arr, getCorrupt arr) |> Array.filter (snd >> ((=)0)) |> Array.map fst

let getCorrupt2 (arr : string array) =
    let rec f opens lst =
        //printfn "Next: %A. LastOpen: %A" (List.tryHead lst) (List.tryHead opens)
        match lst with
        | x :: xs when isOpen x -> f (x :: opens) xs
        | x :: xs when isClose x ->
            if (isCorrectClose (List.head opens) x) then
                f (List.tail opens) xs
            else
                failwithf "."
        | [] -> opens

    f [] (List.ofArray arr)

let ans2 = data

let convert =
    function
    | "{" -> "}" | "<" -> ">" | "[" -> "]" | "(" -> ")" | s -> failwithf "%s" s

let points lst =
    lst
    |> List.fold (fun p c ->
        let r = p * 5I
        match c with
        | ")" -> r + 1I | "]" -> r + 2I | "}" -> r + 3I | ">" -> r + 4I) 0I


let p = 
    incomplete
    |> Array.map getCorrupt2
    |> Array.map (fun lst -> lst |> List.toArray |> Array.map char |> String, List.map convert lst |> points)
    |> Array.sortBy snd

p.Length

5 / 2

p[p.Length / 2]

p
|> Array.iteri (fun i s -> printfn "%i: %A" i s)

ans2