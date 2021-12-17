#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 17

let (tx1,ty1) = (185,-122)
let (tx2,ty2) = (221,-74)

let doIter (pos : int*int,vel : int*int) =
    match (pos,vel) with
    | (pX,pY),(vX,vY) ->
        (pX+vX, pY+vY),(vX-Math.Sign(vX),vY-1)

let makePath start vel' =
    let rec f (pos,vel) path =
        let (newPos,newVel) = doIter (pos,vel)
        match newPos with
        | (x,y) when x >= tx1 && x <= tx2 && y >= ty1 && y <= ty2 ->
            Some (vel',newPos :: path)
        | (_,y) when y < ty1 -> None
        | (x,_) when x > tx2 -> None
        | _ ->
            f (newPos,newVel) (newPos :: path)
    
    f (start,vel') []
    
let vels =
    seq {
        for x in 0..tx2 do
            for y in ty1..1000 do
                yield makePath (0,0) (x,y)
    }
    |> Array.ofSeq
    |> Array.choose id

let ans1 =
    vels
    |> Array.map (snd >> List.maxBy snd)
    |> Array.maxBy snd
    |> snd

let ans2 = vels.Length
