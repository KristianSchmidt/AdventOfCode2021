#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 8

let counts =
    data
    |> Array.map (fun s -> Helpers.split " | " s)
    |> Array.map (Array.last)
    |> Array.collect (fun s -> Helpers.split " " s |> (Array.map String.length))
    |> Array.countBy id
    |> Map.ofArray

let ans1 = counts[2] + counts[3] + counts[4] + counts[7]

ans1

/// Part 2

// uniq: 1 (2 segments), 4 (4 segments), 7 (3 segments), 8 (7 segments)

// Top segment = 7 - 1
// Bottom two left segments = 8 - 4 - top
// Middle segments = intersection of 2,3,5
// Bottom segment = Middle segments - 4 - top
// bottomLeft = 8 - 4 - top - bottom 

let deduce (q : (int*Set<char>) array) =
    let findOnly i = q |> Array.find (fst >> (=)i) |> snd
    let findAll i = q |> Array.filter (fst >> (=)i) |> Array.map snd
    let toChar s = s |> Set.toArray |> Array.head
    let seven = findOnly 3
    let one = findOnly 2
    let four = findOnly 4
    let eight = findOnly 7
    let middles = Set.intersectMany (findAll 5)
    
    let top = (seven - one) |> toChar
    let bottom = Set.difference middles four |> Set.remove top |> toChar
    let middle = middles |> Set.remove top |> Set.remove bottom |> toChar
    let bottomLeft = (eight - four) |> Set.remove top |> Set.remove bottom |> toChar
    let topLeft = (four - one) |> Set.remove middle |> toChar
    let bottomRight = Set.intersect (Set.intersectMany (findAll 6)) one |> toChar
    let topRight =
        Set.difference
            (Set.ofArray [|'a';'b';'c';'d';'e';'f';'g'|])
            (Set.ofArray [|top;middle;bottom;bottomLeft;bottomRight;topLeft|])
        |> toChar

    printfn "Top: %A" top
    printfn "Bottom: %A" bottom
    printfn "Middle: %A" middle
    printfn "BottomLeft: %A" bottomLeft
    printfn "BottomLeft: %A" bottomLeft
    printfn "TopLeft: %A" topLeft
    printfn "BottomRight: %A" bottomRight
    printfn "TopRight: %A" topRight
    
    let answer (s : Set<char>) =
        let subset a = Set.isSubset (Set.ofArray a) s
        match s with
        | _ when subset [|top; topLeft; topRight; middle; bottomLeft; bottomRight; bottom|] -> 8
        | _ when subset [|top; topLeft; topRight; middle; bottomRight; bottom|] -> 9
        | _ when subset [|top; topLeft; topRight; bottomLeft; bottomRight; bottom|] -> 0
        | _ when subset [|top; topLeft; middle; bottomLeft; bottomRight; bottom|] -> 6
        
        | _ when subset [|top; topRight; middle; bottomLeft; bottom|] -> 2
        | _ when subset [|top; topRight; middle; bottomRight; bottom|] -> 3
        | _ when subset [|top; topLeft; middle; bottomRight; bottom|] -> 5

        | _ when subset [|topLeft; topRight; middle; bottomRight|] -> 4
        | _ when subset [|top; topRight; bottomRight;|] -> 7
        | _ when subset [|topRight;bottomRight|] -> 1
        | s -> failwithf "%A" s
        
    answer

let output t =
    let [|q;a|] = Helpers.split " | " t
    let input =
        Helpers.split " " q
        |> Array.map (fun a -> String.length a, Set.ofArray (a.ToCharArray()))
    let answerf = deduce input
    Helpers.split " " a
    |> Array.map (fun s -> s.ToCharArray() |> Set.ofArray)
    |> Array.map answerf
    |> Array.map (string >> char)
    |> (fun a -> new String(a))
    |> int

let ans2 = 
    data
    |> Array.sumBy output

ans2