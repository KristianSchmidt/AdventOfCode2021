#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 16

let toString (arr : char array) = String.Join("", arr)

let conv (c : char) =
    Convert.ToString(Convert.ToInt32(string c, 16),2).PadLeft(4).Replace(" ", "0")
    
let binToInt (s : string) = Convert.ToInt64(s,2)

let parseLiteral (arr : char array) idx =
    let rec f idx' grps =
        let nextFive = arr[idx'..idx'+4]
        let idx'' = idx' + 5
        match nextFive[0] with
        | '0' -> idx'', nextFive :: grps
        | '1' -> f idx'' (nextFive :: grps)
    
    let (endIdx, data) = f idx []
        
    let data' =
        data
        |> List.rev
        |> List.map (Array.skip 1 >> String)
        |> (fun lst -> String.Join("", lst))
        |> (fun s -> Convert.ToInt64(s,2))

    //printfn "Literal %i - StartIdx: %i - EndIdx: %i" data' idx endIdx
    data', endIdx

type EndCondition =
    | RunUntilEnd
    | Packets of int

type Packet =
    | Literal of int64 * int64 * int64
    | Operator of int64 * int64 * char * Packet list

let rec parse (arr : char array) cond' idx' =
    let rec f idx cond packets =
        match cond with
        | RunUntilEnd when idx >= arr.Length - 1 ->
            //printfn "Ending - idx: %i - len: %i" idx (arr.Length - 1) 
            List.rev packets, idx
        | RunUntilEnd when arr[idx..] |> Array.forall ((=)'0') ->
            List.rev packets, idx
        | Packets left when left = 0 ->
            List.rev packets, idx
        | _ ->
            let version = binToInt <| String(arr[idx..idx+2])
            let typeid  = binToInt <| String(arr[idx+3..idx+5])

            //printfn "Idx: %i/%i. Version: %s. Typeid: %s" idx arr.Length version typeid

            let (packet,nextIdx) =
                match typeid with
                | 4L ->
                    let (literal, newIdx) = parseLiteral arr (idx+6)
                    //printfn "Literal %A. NewIdx: %i" literal (newIdx)
                    let lit = Literal (version, typeid, literal)
                    (lit, newIdx)
                | _ -> // operator
                    let lengthTypeId = arr[idx+6]
                    match lengthTypeId with
                    | '0' ->
                        let endOfLength = idx+7+14
                        let length = arr[idx+7..endOfLength] |> toString |> binToInt |> int
                        //printfn "Operator 0 - length: %i" length
                        let newIdx = idx+7+14 + length + 1
                        //printfn "Op0: Starting next itr at %i" (endOfLength+1)
                        let children,_ = parse arr[endOfLength+1..endOfLength+length] RunUntilEnd 0

                        Operator (version, typeid, lengthTypeId, children), newIdx
                    | '1' ->
                        let endOfLength = idx+7+10
                        let numChildren =
                            arr[idx+7..endOfLength] |> toString |> binToInt |> int
                        //printfn "Operator 1 - numChildren: %i" numChildren
                        //printfn "Op1: Starting next itr at %i" (idx+7+10+1)
                        let children,nextIdx = parse arr[endOfLength+1..] (Packets numChildren) 0
                        //printfn "Finished parsing op1"
                        Operator (version, typeid, lengthTypeId, children), (endOfLength+nextIdx+1)
                //| _ -> failwithf "%s" typeid

            let newCond =
                match cond with
                | RunUntilEnd -> cond
                | Packets i -> Packets (i-1)
            
            f nextIdx newCond (packet :: packets)

    f idx' cond' []

let parseString (s : string) =
    s.ToCharArray()
    |> Array.map conv
    |> (fun arr -> String.Join("", arr))
    |> (fun s -> parse (s.ToCharArray()) RunUntilEnd 0)
    |> fst

let tree = parseString data[0]

let getVersions (tree : Packet) =
    let rec f subTree =
        match subTree with
        | Literal (v,_,_) -> v
        | Operator (v,_,_,children) ->
            let childSums = children |> List.sumBy f
            v + childSums

    f tree

let ans1 = getVersions tree[0]

ans1

/// Part 2

let calculateTree (tree : Packet) =
    let rec f subTree =
        match subTree with
        | Literal (_,_,l) -> l
        | Operator (_,typeid,_,children) ->
            let childVals = children |> List.map f
            match typeid with
            | 0L -> List.sum childVals
            | 1L -> List.reduce (*) childVals
            | 2L -> List.min childVals
            | 3L -> List.max childVals
            | 5L -> if (childVals[0] > childVals[1]) then 1L else 0L
            | 6L -> if (childVals[0] < childVals[1]) then 1L else 0L
            | 7L -> if (childVals[0] = childVals[1]) then 1L else 0L

    f tree

let ans2 = calculateTree tree[0]

ans2