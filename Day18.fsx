#load "Helpers.fsx"

open System
open System.Text.RegularExpressions

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 18

type Snailfish =
    | Regular of int
    | Pair of Snailfish * Snailfish

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let strlen (s : string) = s.Length

let parse (s : string) =
    let rec f i =
        let sub = s.Substring(i)
        //printfn "%i - %s" i sub
        match sub with
        | Regex "^\[\[(\d+),(\d+)\],\[(\d+),(\d+)\]\]" [d1;d2;d3;d4] ->
            Pair(Pair(Regular (int d1), Regular (int d2)),
            Pair(Regular (int d3), Regular(int d4))),
            9 + (strlen (d1+d2+d3+d4))
        | Regex "^\[(\d+),\[(\d+),(\d+)\]\]" [d1;d2;d3] ->
            Pair(Regular(int d1), Pair(Regular(int d2), Regular(int d3))),
            6 + (strlen (d1+d2+d3))
        | Regex "^\[\[(\d+),(\d+)\],(\d+)\]" [d1;d2;d3] ->
            Pair(Pair(Regular(int d1), Regular(int d2)), Regular(int d3)),
            6 + (strlen (d1+d2+d3))
        | Regex "^\[(\d+),(\d+)\]" [d1;d2] ->
            Pair(Regular (int d1), Regular (int d2)),
            3 + (strlen (d1+d2))
        | Regex "^(\d+)" [d] -> Regular (int d), (strlen d)
        | Regex "^," [] -> f (i+1)
        | _ ->
            //printfn "Parsing p1 at %i" (i+1)
            let (p1,i1) = f (i+1)
            //printfn "P1: %A" p1
            //printfn "Parsing p2 at %i" (i+1+i1+1)
            let (p2,i2) = f (i+1+i1+1)
            //printfn "P2: %A" p2
            Pair (p1,p2), i1+i2+3
        | _ -> failwithf "%s" sub

    f 0 |> fst

let nums =
    data
    |> Array.map parse

let add p1 p2 = Pair(p1,p2)

nums[4]

//let explode num' =
//    let rec f num repLeft repRight depth = 
//        match num, repLeft, repRight with
//        | Pair(Regular d1, Regular d2), None, None when depth >= 4 ->
//            Some d1, Some d2, Regular 0
//        | Pair(p1,p2), _,_ ->
//            let (newP1,left,right) = f p1 repLeft repRight (depth + 1)
//            let (newP2,)
//            
//            
//            match f p1 (depth + 1) with
//            | Some x -> Some x
//            | None -> f p2 (depth + 1)
//        | Regular _ -> None
//
//    f num' 0

parse "[[[[[9,8],1],2],3],4]"
//|> explode

let findExp (s : string) =
    let arr = s.ToCharArray()
    let len = arr.Length
    let rec f i depth =
        if (i >= len) then None
        else
        let sub = s.Substring(i)
        //printfn "%i - %s" depth sub
        match sub with
        | Regex "^\[(\d+),(\d+)\]" [d1;d2] when depth >= 4 ->
            //printfn "%A" (i,int d1,int d2)
            Some (i,int d1,int d2)
        | _ ->
            let c = arr[i]
            match c with
            | '[' -> f (i+1) (depth+1)
            | ']' -> f (i+1) (depth-1)
            | _ -> f (i+1) depth

    f 0 0

findExp "[[[[1,[9,8]],2],3],4]"
findExp "[[[[0,[12,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]],[2,9]]"
findExp "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]"

findExp "[[[[7,0],[8,8]],[[8,[12,13]],[15,0]]],[14,[8,[9,0]]]]"

let isDigit (c : char) = (int c) >= 48 && (int c) <= 57

let replaceExp (s : string) i (l,r) =
    let arr = s.ToCharArray()
    let firstPart = arr |> Array.take i
    let lastOffset = (i+3+(strlen $"{l}{r}"))
    let lastPart = arr |> Array.skip lastOffset

    let firstIdx =
        firstPart |> Array.tryFindIndexBack isDigit
    
    let lastIdx =
        lastPart |> Array.tryFindIndex isDigit// |> Option.map ((+)(i+5))

    //printfn "%A" lastIdx

    let first =
        match firstIdx with
        | None -> firstPart
        | Some idx ->
            let firstIsNotDigit =
                firstPart |> Array.take idx |> Array.findIndexBack (isDigit >> not)
            let num,numLen =
                match s.Substring(firstIsNotDigit+1) with
                | Regex "^(\d+)" [d] -> int d, d.Length

            let sumArr = ((num+l) |> string).ToCharArray()
            firstPart |> Array.removeManyAt (firstIsNotDigit+1) numLen |> Array.insertManyAt (firstIsNotDigit+1) sumArr

    let last =
        match lastIdx with
        | None -> lastPart
        | Some idx ->
            let num,numLen =
                match s.Substring(lastOffset + idx) with
                | Regex "^(\d+)" [d] -> int d, d.Length

            let sumArr = ((num+r) |> string).ToCharArray()

            lastPart |> Array.removeManyAt idx numLen |> Array.insertManyAt idx sumArr           

    String(first) + "0" + String(last)

let explode (s : string) =
    match findExp s with
    | Some (i,l,r) ->
        Some (replaceExp s i (l,r))
    | None -> None

explode "[[[[7,0],[8,8]],[[8,[12,13]],[15,0]]],[14,[8,[9,0]]]]"

explode "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"
|> Option.bind explode

explode "[[[[7,7],[0,6]],[[6,[7,7]],[11,0]]],[[5,[2,11]],[[0,7],[10,0]]]]"

let split (s : string) =
    let arr = s.ToCharArray()
    let wins = arr |> Array.windowed 2
    let idx = wins |> Array.tryFindIndex (Array.forall isDigit)
    match idx with
    | Some i ->
        let num = String(wins[i]) |> float
        let (l,r) =
            (floor (num / 2.)
             |> int |> string |> (fun s -> s.ToCharArray())
             , ceil(num /2.) |> int |> string |> (fun s -> s.ToCharArray()))
        //printfn "%f" num
        
        arr
        |> Array.removeManyAt i 2
        |> Array.insertManyAt i (Array.concat [|[|'['|];l;[|','|];r;[|']'|]|])
        |> String
        |> Some
    | None -> None

let print (n : Snailfish) =
    let rec f num =
        match num with
        | Regular n -> string n
        | Pair(p1,p2) ->
            let p1s = f p1
            let p2s = f p2
            $"[{p1s},{p2s}]"

    f n

let split2 (s : string) =
    let num = parse s
    let rec f num stop =
        if (stop = true) then
            num,true
        else
            match num with
            | Regular n when n >= 10 ->
                let l = (float n) / 2. |> floor |> int
                let r = (float n) / 2. |> ceil |> int
                Pair(Regular l, Regular r), true
            | Regular n ->
                Regular n, false
            | Pair(p1,p2) ->
                let (p1n,p1stop) = f p1 stop
                let (p2n,p2stop) = f p2 p1stop
                Pair(p1n,p2n),p2stop

    let (n',stopped) = f num false
    if (stopped = true) then
        Some (n' |> print)
    else
        None

let addS p1 p2 = $"[{p1},{p2}]"

let rec reduce (s : string) =
    //printfn "%s" s
    let expOpt = explode s
    match expOpt with
    | Some s' -> 
        //printf "E: "
        reduce s'
    | None ->
        let splitOpt = split2 s
        match splitOpt with
        | Some s'' ->
            //printfn "R: "
            reduce s''
        | None -> s

(Array.tail data)
|> Array.fold (fun s t ->
    addS s t |> reduce
    ) data[0]

addS data[0] data[1]
|> reduce
|> addS data[2]
|> reduce

let testData = """[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]"""

let testArr = testData |> Helpers.split "\n"

addS testArr[0] testArr[1]
|> reduce
|> (fun s -> addS s testArr[2])
|> reduce

let addGrp (data : string array) =
    let rec f s i =
        if (i >= data.Length) then
            s
        else
            //printfn "0: %s" s
            //printfn "1: %s" data[i]
            let next = addS s data[i] |> reduce
            f next (i+1)

    f data[0] 1


let rec calcMagnitude n =
    match n with
    | Pair(p1,p2) -> 3L*(calcMagnitude p1) + 2L * (calcMagnitude p2)
    | Regular n -> int64 n

addGrp data
|> parse
|> calcMagnitude

calcMagnitude (parse "[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]")


addS "[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]" "[2,9]"
|> reduce

addS "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]" "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]"
|> reduce


(Array.tail testArr)
|> Array.fold (fun s t ->
    addS t s |> reduce
    ) testArr[0]

parse "[[2,[[7,4],[8,0]]],[[[[10,4],0],5],[3,[8,5]]]]"

reduce "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"

(replaceExp "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]" 16 (8,4))

int '9'

let n = 14
(float n) / 2. |> floor |> int
(float n) / 2. |> ceil |> int

addS "[[[[4,3],4],4],[7,[[8,4],9]]]" "[1,1]"
|> reduce

addS "[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]" "[7,[5,[[3,8],[1,4]]]]"
|> reduce

addS "[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]" "[2,9]"
|> (fun s -> reduce s 10)

explode "[[[[7,0],[8,8]],[[8,[12,13]],[15,0]]],[14,[8,[9,0]]]]"


let test2 = """[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
[6,6]"""

let test2data = test2 |> Helpers.split "\n"

(Array.tail test2data)
|> Array.fold (fun s t ->
    addS s t |> (fun s' -> reduce s' 1000)
    ) test2data[0]


let dataLen = data.Length - 1

seq {
    for i in 0 .. dataLen do
        for j in 0 .. dataLen do
            if (i <> j) then
                yield (data[i], data[j])
}
|> Seq.toArray
|> Array.maxBy (fun (n1,n2) -> addS n1 n2 |> reduce |> parse |> calcMagnitude)
|> (fun (n1,n2) -> addS n1 n2 |> reduce |> parse |> calcMagnitude)

let ans1 = data

ans1

/// Part 2

let ans2 = data

ans2