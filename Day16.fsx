#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 16

let conv (c : char) =
    Convert.ToString(Convert.ToInt32(string c, 16),2).PadLeft(4).Replace(" ", "0")
    
let extraForMul4 (i : int) =
    (4 - (i % 4)) % 4

let binToInt (s : string) = Convert.ToInt32(s,2)

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
        |> (fun s -> Convert.ToInt32(s,2))

    let nextIdx = extraForMul4 (endIdx - (idx + 6)) + endIdx

    data', nextIdx

[|0..1|]

let parseOperatorType0 (arr : char array) idx =
    let length = arr[idx..idx+14] |> (fun arr -> String.Join("", arr)) |> binToInt
    
    let packetsOfSize11 = (length / 11) - 1
    let lastSize = 11 + (length % 11)
    printfn "%i - %i" packetsOfSize11 lastSize

    let subStartIdx = idx+15
    
    let packetSizes =
        Array.create packetsOfSize11 11
        |> Array.append [|lastSize|]
        |> Array.rev

    subStartIdx + length - 1

parseOperatorType0 ("0000000000110111101000101001010010001001000000000".ToCharArray()) 0

let parseOperatorType1 (arr : char array) idx =
    let numSubPackets =
        arr[idx..idx+10] |> (fun arr -> String.Join("", arr)) |> binToInt

    idx + 11 + (numSubPackets * 11) - 1

type X =
    | Literal of int * int * int
    | Noop

let parse (arr : char array) idx' =
    let rec f idx vs =
        let version = String(arr[idx..idx+2])
        let typeid = String(arr[idx+3..idx+5])

        let (_,nextIdx) =
            match typeid with
            | "100" -> // typeid = 4 literal
                let (literal, newIdx) = parseLiteral arr (idx+6)
                printfn "%A" literal
                let lit = Literal (binToInt version, binToInt typeid, literal)
                (lit, newIdx)
            | _ -> // typeid = 6 operator
                let lengthTypeId = arr[idx+6]
                match lengthTypeId with
                | '0' ->
                    let newIdx = parseOperatorType0 arr (idx+7)
                    Noop, newIdx
                | '1' ->
                    let newIdx = parseOperatorType1 arr (idx+7)
                    Noop, newIdx
            //| _ -> failwithf "%s" typeid

        printfn "NextIdx: %i" nextIdx
        printfn "Length: %i" arr.Length

        if (nextIdx >= arr.Length - 1) then
            (version :: vs)
        else
            f nextIdx (version :: vs)

    f idx' []

let parseString (s : string) =
    s.ToCharArray()
    |> Array.map conv
    |> (fun arr -> String.Join("", arr))
    |> (fun s -> parse (s.ToCharArray()) 0)

parseString data[0]

parseString "8A004A801A8002F478"

data[0].ToCharArray()
|> Array.map conv

// 3 bits = packet version
// 3 bits = packet type id
// packet type id 4 = literal value
//

let ans1 = data

ans1

/// Part 2

let ans2 = data

ans2