#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 8

data
|> Array.map (fun s -> Helpers.split " | " s)
|> Array.map (Array.last)
|> Array.collect (fun s -> Helpers.split " " s |> (Array.map String.length))
|> Array.countBy id

let ans1 = 132 + 125 + 124 + 133

ans1

/// Part 2

//let [|q;a|] = Array.head data |> Helpers.split " | "

// uniq: 1 (2 segments), 4 (4 segments), 7 (3 segments), 8 (7 segments)

// Top segment = 7 - 1
// Bottom two left segments = 8 - 4 - top
// Middle segments = intersection of 2,3,5
// Bottom segment = Middle segments - 4 - top
// bottomLeft = 8 - 4 - top - bottom 

//let test =
//    Helpers.split " " q
//    |> Array.map (fun a -> String.length a, Set.ofArray (a.ToCharArray()))

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
    printfn "Top: %A" top
    let bottom = Set.difference middles four |> Set.remove top |> toChar
    printfn "Bottom: %A" bottom
    let middle = middles |> Set.remove top |> Set.remove bottom |> toChar
    printfn "Middle: %A" middle
    let bottomLeft = (eight - four) |> Set.remove top |> Set.remove bottom |> toChar
    printfn "BottomLeft: %A" bottomLeft
    let topLeft = (four - one) |> Set.remove middle |> toChar
    printfn "TopLeft: %A" topLeft
    let bottomRight = Set.intersect (Set.intersectMany (findAll 6)) one |> toChar
    printfn "BottomRight: %A" bottomRight
    let topRight =
        Set.difference
            (Set.ofArray [|'a';'b';'c';'d';'e';'f';'g'|])
            (Set.ofArray [|top;middle;bottom;bottomLeft;bottomRight;topLeft|])
        |> toChar
    printfn "TopRight: %A" topRight

    printfn "%A" [|top; topLeft; topRight; middle; bottomLeft; bottomRight; bottom|]
    
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
    //let t = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
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

data
|> Array.sumBy output

let data2 = """be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"""

data2
|> (fun s -> s.Split("\n"))
|> Array.map output
|> Array.iter (printfn "%i")

deduce test

let ans2 = data

ans2