#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

//let data = Helpers.Web.getInput 21

let (p1pos,p2pos) = (1,10)

let rec wrap100 = (fun i -> if (i > 100) then wrap100 (i-100) else i)

let rec wrap10 = (fun i -> if (i > 10) then wrap10 (i-10) else i)

let play (p1',p2') =
    let rec f (pos : Map<int,int>) turn (score : Map<int,int>) die i =
        let nextThree = [|die..die+2|] |> Array.map wrap100 

        let nextPlayerPos = (pos[turn] + (Array.sum nextThree)) |> wrap10
        let nextPlayerScore = (score[turn] + nextPlayerPos)

        let nextScore = score |> Map.add turn nextPlayerScore

        let nextPos = pos |> Map.add turn nextPlayerPos

        let nextDie = wrap100 (die + 3)

        //printfn "%i: %A - %i - %i" turn nextThree score[turn] nextPlayerPos

        if (nextPlayerScore >= 1000) then
            nextScore, i*3
        else
            f nextPos (if (turn = 1) then 2 else 1) nextScore nextDie (i+1)

    f (Map.ofArray [|(1,p1');(2,p2')|]) 1 (Map.ofArray [|(1,0);(2,0)|]) 1 1

let ans1 =
    play (p1pos,p2pos)
    |> (fun (m, dRolls) -> (m.Values |> Seq.min)*dRolls)

ans1

/// Part 2

let scoreDist =
    Array.allPairs (Array.allPairs [|1..3|] [|1..3|]) [|1..3|]
    |> Array.map (fun ((x,y),z) -> x+y+z)
    |> Array.countBy id
    |> Array.map (fun (x,y) -> x, int64 y)

type State =
    { Pos : Map<int,int>; Score : Map<int,int>; }

let expandState turn ((s,c) : State*int64) =
    scoreDist
    |> Array.map (fun (roll,count) ->
            let newPos = wrap10 (s.Pos[turn] + roll)
            let newScore = s.Score[turn] + newPos
            {
                Pos = s.Pos |> Map.add turn newPos
                Score = s.Score |> Map.add turn newScore
            }, count*c
        )

let checkState (s : State, count : int64) =
    if (s.Score[1] >= 21) then
        Some (1,count)
    else if (s.Score[2] >= 21) then
        Some (2,count)
    else
        None

let play2 (p1',p2') =
    let initState = { Pos = (Map.ofArray [|(1,p1');(2,p2')|]); Score = (Map.ofArray [|(1,0);(2,0)|]) }

    let rec f (states : (State*int64) array) allWins turn =
        if (Array.length states = 0) then
            allWins
        else
        let nextStates =
            states
            |> Array.collect (expandState turn)
            |> Array.groupBy fst
            |> Array.map (fun (s, arr) -> s, arr |> Array.sumBy snd)
        
        let wins = nextStates |> Array.choose (fun (s,c) -> checkState (s,c) |> Option.map (fun x -> s,x))
        let winStates = wins |> Array.map fst |> Set.ofArray
        let nextStates2 = nextStates |> Array.filter (fun (s,c) -> Set.contains s winStates |> not)

        f nextStates2 (wins :: allWins) (if turn = 1 then 2 else 1)

    f ([|initState, 1L|]) [] 1

let ans2 =
    play2 (p1pos,p2pos)
    |> Array.ofList
    |> Array.collect id
    |> Array.map snd
    |> Array.groupBy fst
    |> Array.map (fun (i,arr) -> i, arr |> Array.sumBy snd)
    |> Array.maxBy snd
    |> snd

ans2