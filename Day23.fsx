#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

type Position = | Hall of int | Room of string * int

type State = Map<Position,string>

let data = Helpers.Web.getInput 23

let data2 = data |> Array.insertManyAt 3 [|"  #D#C#B#A#"; "#D#B#A#C#"|]

let exampleData = """#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########"""

let mkRoom (data : string array) i =
    Helpers.split "#" data[i+2]
    |> Array.filter (String.IsNullOrWhiteSpace >> not)
    |> Array.zip [|'A'..'D'|]
    |> Array.map (fun (r,letter) -> Room (string r, i), letter)

let exData =
    let data = Helpers.split "\n" exampleData
    
    Array.concat [| mkRoom data 0; mkRoom data 1 |]
    |> Map.ofArray

(*
Data structure

Hallway positions
Hall0 - all the way to the left
Hall1 - almost to the left
Hall3 - To the right of the first room
Hall5
Hall7
Hall9
Hall10 - all the way to the right

RoomA0, RoomA1
RoomB0, RoomB1
RoomC0, RoomC1
RoomD0, RoomD1
*)

let isCorrect size (s : State) =
    let hasCorrectKey l i =
        s |> Map.tryFind (Room (l,i))
          |> Option.map ((=)l)
          |> Option.defaultValue false
    
    Array.allPairs [|"A";"B";"C";"D"|] [|0..size-1|]
    |> Array.forall (fun (l,i) -> hasCorrectKey l i)

let initState =
    Array.concat [| mkRoom data 0; mkRoom data 1 |]
    |> Map.ofArray

let initState2 =
    Array.concat [| mkRoom data2 0; mkRoom data2 1; mkRoom data2 2; mkRoom data2 3  |]
    |> Map.ofArray

let hallwayPos =
    function
    | "A" -> 2 | "B" -> 4
    | "C" -> 6 | "D" -> 8

let isOddOrAbove i = (i % 2 = 1) || i > 9
let isOddOrBelow i = (i % 2 = 1) || i < 1

let private getFreeHallways r isFree =
    let left =
        [0..((hallwayPos r)-1)]
        |> List.filter isOddOrBelow
        |> List.map Hall
        |> List.rev
        |> List.takeWhile isFree
        |> List.rev
    let right =
        [(hallwayPos r)+1 .. 10]
        |> List.filter isOddOrAbove
        |> List.map Hall
        |> List.takeWhile isFree

    List.concat [|left;right|]

let moves size (pos : Position) (s : State) =
    let occupied = s |> Map.keys |> Set.ofSeq
    let isTaken x = Set.contains x occupied
    let isFree = isTaken >> not
    match pos with
    | Room (r, j) ->
        // Don't move if you and all below are correct
        // Only move if all above are free
        let allCorrect =
            [|j..size-1|] |> Array.forall (fun j' -> s[Room(r,j')] = r)

        if (not allCorrect) then
            let allAboveFree =
                [|0..j-1|] |> Array.forall (fun j' -> isFree (Room(r,j')))
            if (allAboveFree) then
                getFreeHallways r isFree
            else
                []
        else
            []
        
    | Hall i ->
        let myLetter = s[pos]
        // Can only move into the bottom of own cave
        let dest = hallwayPos myLetter
        let canMove =
            if (i < dest) then
                [|i+1..dest|] |> Array.forall (Hall >> isFree)
            else
                [|dest..i-1|] |> Array.forall (Hall >> isFree)
        if (canMove) then
            // Can move to the first one that's free from the bottom
            let firstFree =
                [|0..size-1|]
                |> Array.tryFindBack (fun j -> isFree (Room (myLetter, j)))

            match firstFree with
            | Some j ->
                // All above must be free, but I guess that happens naturally
                // from limiting the moves?
                // All below must be the same letter
                let allBelowCorrect =
                    if (j < size-1) then
                        [|j+1..size-1|]
                        |> Array.forall (fun j' -> s[Room (myLetter, j')] = myLetter)
                    else
                        true

                if (allBelowCorrect) then
                    [Room(myLetter, j)]
                else
                    []

            | None -> []
        else
            []
        
let keyCost =
    function
    | "A" -> 1 | "B" -> 10
    | "C" -> 100 | "D" -> 1000

let rec cost (start,dest,key) =
    match start, dest with
    | Room (l, i), Hall h
    | Hall h, Room (l, i) ->
        let dist = abs (h - hallwayPos l)
        (dist + i + 1) * keyCost key

let solve size initState =
    let rec f queue bestSol =
        let bestCost = bestSol |> Option.defaultValue Int32.MaxValue
        match queue with
        | [] -> bestSol
        | (x,c,m) :: xs ->
            //if (c + heuristic x > bestCost) then
            if (c > bestCost) then
                f xs bestSol // Exit early if curr cost > best found
            else
                let nextMoves =
                    x
                    |> Map.toList
                    |> List.collect (fun (k,v) -> moves size k x |> List.map (fun p -> k, v, p, c + cost (k,p,v)))
                    
                let nextStates =
                    nextMoves
                    |> List.map (fun (from, letter, to', c') -> x |> Map.remove from |> Map.add to' letter, c', m+1)

                // Check for doneness if nextMoves is empty
                let nextQueue =
                    nextStates
                    |> List.fold (fun s t -> t :: s) xs
                    //|> List.sortByDescending (fun (_,c,_) -> c)

                //printfn "Queue size: %i" (List.length nextQueue)
                //printfn "Moves done at head: %i" (List.head nextQueue |> fun (_,_,m) -> m)

                if (nextMoves = [] && isCorrect size x && c < bestCost) then
                    printfn "New best: %i" c
                    f nextQueue (Some c)
                else
                    f nextQueue bestSol

    f [initState,0,0] None // (Some 13418)

#time "on"
solve 2 exData

solve 2 initState

// best 13417 too high
// best = 11417

let ans1 = 11417

ans1

/// Part 2

solve 4 initState2

let ans2 = data

ans2