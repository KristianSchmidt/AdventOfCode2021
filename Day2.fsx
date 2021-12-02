#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

type Dir = | Forward of int
           | Down of int 
           | Up of int

let data =
    Helpers.Web.getInput 2
    |> Array.map (fun s -> match Helpers.split " " s with
                           | [|"forward"; i|] -> Forward (int i)
                           | [| "down"; i|] -> Down (int i)
                           | [| "up"; i |] -> Up (int i)
                           )

let (x1,y1) =
    data
    |> Array.fold (fun (x,y) t -> match t with
                                  | Forward i -> (x+i,y)
                                  | Down i -> (x,y+i)
                                  | Up i -> (x,y-i)
                                  ) (0,0)

let ans1 = x1*y1

ans1

/// Part 2

let (x2,y2,_) =
    data
    |> Array.fold (fun (x,y,a) t -> match t with
                                    | Forward i -> (x+i,y+(a*i),a)
                                    | Down i -> (x,y,a+i)
                                    | Up i -> (x,y,a-i)
                                  ) (0,0,0)


let ans2 = (x2*y2)

ans2