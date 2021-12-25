#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 24

type Literal =
    | Var of string
    | Num of int64

let printLit = function | Var s -> s | Num i -> string i

type Op =
    | Input of string*int
    | Add of Literal*Literal
    | Mul of Literal*Literal
    | Div of Literal*Literal
    | Mod of Literal*Literal
    | Eql of Literal*Literal

let printOp =
    function
    | Input(s,i) -> $"{s} = i[{i}]"
    | Add (Var s, l) -> $"{s} = {s} + {printLit l}"
    | Mul (Var s, l) -> $"{s} = {s} * {printLit l}"
    | Div (Var s, l) -> $"{s} = {s} / {printLit l}"
    | Mod (Var s, l) -> $"{s} = {s} %% {printLit l}"
    | Eql (Var s, l) -> $"{s} = {s} eql {printLit l}"

let parseInstr i (s : string) =
    let arr = Helpers.split " " s
    if (arr.Length = 2) then
        Input (arr[1],i)
    else
        let lit2 =
            let (succ,y) = Int32.TryParse(arr[2])
            if (succ) then Num y else Var arr[2]
        let lit1 = Var arr[1]
        match arr[0] with
        | "add" -> Add (lit1, lit2)
        | "mul" -> Mul (lit1, lit2)
        | "div" -> Div (lit1, lit2)
        | "mod" -> Mod (lit1, lit2)
        | "eql" -> Eql (lit1, lit2)

let parseProgram (arr : string array) =
    let rec f acc currIdx inputCount =
        if (currIdx > arr.Length - 1) then
            acc |> List.rev |> List.toArray
        else
            let next = parseInstr inputCount arr[currIdx]
            match next with
            | Input _ -> f (next :: acc) (currIdx+1) (inputCount+1)
            | _ -> f (next :: acc) (currIdx+1) inputCount

    f [] 0 0
    
let program = parseProgram data

let getInstructionsForInput i (arr : Op array) =
    let inputIdx = arr |> Array.findIndex ((=)<|Input("w",i))
    let next = arr
               |> Array.tryFindIndex ((=)<|Input("w",i+1))
               |> Option.map (fun j -> j - 1)
               |> Option.defaultValue (arr.Length - 1)
    arr[inputIdx..next]

(*
General digit k: 
w = i[k]
if (z[k-1] % 26 + xAdd <> i[k]) then
    z[k] = (z[k-1] / zDiv) * 26 + i[k] + yAdd
else
    z[k] = (z[k-1] / zDiv)
*)

getInstructionsForInput 0 program
|> Array.iter (printOp >> printfn "%s")

let getXadd ops =
    ops |> Array.pick (function
                       | Add(Var "x", Num i) -> Some i | _ -> None)

let getWadd ops =
    ops |> Array.rev
        |> Array.pick (function
                       | Add(Var "y", Num i) -> Some i | _ -> None)

let getZdiv ops =
    ops |> Array.pick (function
                       | Div(Var "z", Num i) -> Some i | _ -> None)

let general = """w = i[k]
if (z[k-1] % 26 + xAdd <> i[k]) then
    z[k] = (z[k-1] / zDiv) * 26 + i[k] + wAdd
else
    z[k] = (z[k-1] / zDiv)"""

let printProgram i =
    let ops = getInstructionsForInput i program
    let (xadd, wadd, zdiv) = getXadd ops, getWadd ops, getZdiv ops
    general.Replace("k-1", string (i-1)).Replace("[k]",$"[{i}]")
           .Replace("xAdd", string xadd).Replace("zDiv", string zdiv)
           .Replace("wAdd", string wadd)

printProgram 7

let execute2 (xadd, zdiv, wadd) (ik : int64) (z : int64) =
    if (z % 26L + xadd <> ik) then
        (z / zdiv) * 26L + ik + wadd
    else
        (z / zdiv)

let tryShortPath (xadd, zdiv, wadd) (z : int64) =
    let sol = z % 26L + xadd
    if (sol >= 1L && sol <= 9L) then Some sol else None

let ex2programs =
    [|0..13|]
    |> Array.map (fun i -> let ops = getInstructionsForInput i program
                           execute2 (getXadd ops, getZdiv ops, getWadd ops))

let shortpaths =
    [|0..13|]
    |> Array.map (fun i -> let ops = getInstructionsForInput i program
                           tryShortPath (getXadd ops, getZdiv ops, getWadd ops))

let genSol () =
    let rec f i z nums =
        if (i = 14) then
            if (z = 0L) then Some (List.rev nums) else None
        else
            let chosenInput =
                match shortpaths[i] z with
                | Some k -> k
                | None -> Helpers.random 1 9 |> int64

            f (i+1) (ex2programs[i] chosenInput z) (chosenInput :: nums)

    f 0 0L []

let (ans1,ans2) =
    let sols = [1..10_000_000]
               |> List.choose (fun _ -> genSol ())
               |> List.map (fun arr -> arr |> List.map string |> (fun a -> String.Join("",a)))
    (sols |> List.sortDescending |> List.head),
    (sols |> List.sort |> List.head)
