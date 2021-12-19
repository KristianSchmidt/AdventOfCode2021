#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    let input = Helpers.Web.getInput 19
    String.Join('\n', input |> Array.filter ((<>)""))
    |> Helpers.split "---"
    |> Array.filter (fun s -> s.Contains("scanner") |> not)
    |> Array.map (fun s -> s.Trim())
    |> Array.filter ((<>)"")
    |> Array.map (Helpers.split "\n")
    |> Array.map (Array.map (Helpers.split ","))
    |> Array.map (Array.map ((fun [|x;y;z|] -> (int x, int y, int z))))

let example =
    System.IO.File.ReadAllText("Day19example.txt")
    |> Helpers.split "---"
    |> Array.filter (fun s -> s.Contains("scanner") |> not)
    |> Array.map (fun s -> s.Trim())
    |> Array.filter ((<>)"")
    |> Array.map (Helpers.split "\n")
    |> Array.map (Array.map (Helpers.split ","))
    |> Array.map (Array.map ((fun [|x;y;z|] -> (int x, int y, int z))))

type Axis = | X | Y | Z

let rotate axis pos deg' (x,y,z) =
    let deg = if (deg' = 360) then 0 else deg'
    match axis,deg with
    | X,0   -> pos*x,y,z
    | X,90  -> pos*x,-z,y
    | X,180 -> pos*x,-y,-z
    | X,270 -> pos*x,z,-y
    | Y,0   -> x,pos*y,z
    | Y,90  -> -z,pos*y,x
    | Y,180 -> -x,pos*y,-z
    | Y,270 -> z,pos*y,-x
    | Z,0   -> x,y,pos*z
    | Z,90  -> -y,x,pos*z
    | Z,180 -> -x,-y,pos*z
    | Z,270 -> y,-x,pos*z

let rotateRev (axis,pos,deg') coord = rotate axis pos (360-deg') coord

let transforms =
    seq {
        for pos in [-1;1] do
            // x
            yield ((X,pos,0),(fun (x,y,z) -> pos*x,y,z))
            yield ((X,pos,90),(fun (x,y,z) -> pos*x,-z,y))
            yield ((X,pos,180),(fun (x,y,z) -> pos*x,-y,-z))
            yield ((X,pos,270),(fun (x,y,z) -> pos*x,z,-y))

            // y
            yield ((Y,pos,0),(fun (x,y,z) -> x,pos*y,z))
            yield ((Y,pos,90),(fun (x,y,z) -> -z,pos*y,x))
            yield ((Y,pos,180),(fun (x,y,z) -> -x,pos*y,-z))
            yield ((Y,pos,270),(fun (x,y,z) -> z,pos*y,-x))

            // z
            yield ((Z,pos,0),(fun (x,y,z) -> x, y, pos*z))
            yield ((Z,pos,90),(fun (x,y,z) -> -y,x, pos*z))
            yield ((Z,pos,180),(fun (x,y,z) -> -x,-y, pos*z))
            yield ((Z,pos,270),(fun (x,y,z) -> y,-x,  pos*z))
    }
    |> Array.ofSeq

let allTransforms data =
    transforms
    |> Array.map (fun (x,f) -> x, Array.map f data)

let sub ((x,y,z),(x',y',z')) = (x-x',y-y',z-z')
let add ((x,y,z),(x',y',z')) = (x+x',y+y',z+z')

let findLoc baseScanner newScanner =
    let trials =
        allTransforms newScanner
        |> Array.map (fun (info,arr) -> info, arr, Array.allPairs baseScanner arr
                                                   |> Array.map sub)

    trials
    |> Array.map (fun (info,arr,trial) -> info, arr, trial |> Array.countBy id |> Array.maxBy snd)
    |> Array.maxBy ((fun (info,arr,trial) -> snd trial))


let findLoc2 p1 p2 =
    seq {
        for (i1,i) in allTransforms p1 do
            for (i2,j) in allTransforms p2 do
                yield (i1,i2,j,Array.allPairs i j
                             |> Array.map sub
                             |> Array.countBy id
                             |> Array.maxBy snd)
    }
    |> Array.ofSeq

let applyRotations (xs : (Axis*int*int) list) loc =
    xs |> List.fold (fun s t -> rotateRev t s) loc

let findVec p1 p2 rotations locSource =
    let (i1,i2,j,(loc,_)) =
        findLoc2 p1 p2
        |> Array.filter (fun (i1,i2,_,(loc,cnt)) -> cnt >= 12)
        |> Array.maxBy (fun (i1,i2,_,(loc,cnt)) -> cnt)

    //findLoc2 p1 p2
    //|> Array.filter (fun (i1,i2,_,(loc,cnt)) -> cnt >= 12)
    //|> Array.iter (fun (i1,i2,_,(loc,cnt)) -> printfn "Rotation: %A %A" i1 i2)

    let rotateRel0 loc = rotateRev i1
                         >> applyRotations rotations
                         >> (fun c -> add (c,loc))
        
    let newRots = [i2;i1]
    let newLoc = rotateRel0 locSource loc
    let beaconsRel0 = j |> Array.map (rotateRel0 newLoc)
    newRots, newLoc, beaconsRel0
    
//let loc0 = (0,0,0)
//let (rot1, loc1, bec1) = findVec example[0] example[1] [] loc0
//let (rot4, loc4, bec4) = findVec example[1] example[4] rot1 loc1
//let (rot3, loc3, bec3) = findVec example[1] example[3] rot1 loc1
//let (rot2, loc2, bec2) = findVec example[4] example[2] rot4 loc4

let loc0 = (0,0,0)
let (rot1, loc1, _) = findVec data[0] data[1] [] loc0
let (rot9, loc9, _) = findVec data[0] data[9] [] loc0
let (rot141, loc141, bec141) = findVec data[1] data[14] rot1 loc1
let (rot142, loc142, bec142) = findVec data[9] data[14] rot9 loc9



let solve data (pairMap : Map<int,int array>) =
    let rec f visited (locs : Map<int,int*int*int>) (rots : Map<int,(Axis*int*int) list>) beacons =
        if (Set.count visited = Array.length data) then
            beacons
        else
            // find something from visited's pairMap that has
            // not been seen yet
            let (source,target) =
                visited
                |> Set.toArray
                |> Array.collect (fun c -> pairMap[c]
                                           |> Array.filter (fun x -> Set.contains x visited |> not)
                                           |> Array.map (fun x -> (c,x)))
                |> Array.head

            let st =
                visited
                |> Set.toArray
                |> Array.collect (fun c -> pairMap[c]
                                           |> Array.filter (fun x -> Set.contains x visited |> not)
                                           |> Array.map (fun x -> (c,x)))
                |> Array.filter (snd >> ((=)target))


            for (s,t) in st do
                let (_, loc, _) = findVec data[s] data[t] rots[s] locs[s]
                printfn "%i->%i: %A" s t loc

            printfn "Source,target = (%i,%i)" source target
            let (rot, loc, bec) = findVec data[source] data[target] rots[source] locs[source]
            
            let newBecs = Set.union beacons (Set.ofArray bec)
            let newLoc = Map.add target loc locs
            let newRots = Map.add target rot rots
            let newVisited = Set.add target visited
            f newVisited newLoc newRots newBecs

    let initVisited = set [0]
    let initLocs = Map.ofList [0,(0,0,0)]
    let initRots = Map.ofList [0,[]]
    let initBecs = Set.ofArray data[0]
    f initVisited initLocs initRots initBecs


let isPair p1 p2 =
    seq {
        for i in allTransforms p1 |> Array.map snd do
            for j in allTransforms p2 |> Array.map snd do
                yield Array.allPairs i j
                      |> Array.map sub
                      |> Array.countBy id
                      |> Array.maxBy snd
    }
    |> Array.ofSeq
    |> Array.maxBy snd
    |> snd |> (fun x -> x >= 12)

let allPairsData =
    seq {
        for i in 0..data.Length-1 do
            for j in 0..data.Length-1 do
                if (i < j) then
                    printfn "%i,%i" i j
                    yield (i,j,isPair data[i] data[j])
    }
    |> Array.ofSeq
    |> Array.filter (fun (_,_,x) -> x)

let allPairsExample =
    seq {
        for i in 0..example.Length-1 do
            for j in 0..example.Length-1 do
                if (i < j) then
                    yield (i,j,isPair example[i] example[j])
    }
    |> Array.ofSeq
    |> Array.filter (fun (_,_,x) -> x)

let pairMapExample =
    allPairsExample
    |> Array.collect (fun (x,y,_) -> [|(x,y);(y,x)|])
    |> Array.groupBy fst
    |> Array.map (fun (c,arr) -> c,arr |> Array.map snd)
    |> Map.ofArray

solve example pairMapExample
|> Set.toArray
|> Array.sort
|> Array.iter (printfn "%A")

let pairMap =
    allPairsData
    |> Array.collect (fun (x,y,_) -> [|(x,y);(y,x)|])
    |> Array.groupBy fst
    |> Array.map (fun (c,arr) -> c,arr |> Array.map snd)
    |> Map.ofArray

let pairMap2 =
    allPairsData
    |> Array.collect (fun (x,y,_) -> [|(x,y)|])
    |> Array.groupBy fst
    |> Array.map (fun (c,arr) -> c,arr |> Array.map snd)
    |> Map.ofArray

solve data pairMap
|> Set.count
    
// 605 too high

data[0]

let ans1 = data

ans1

/// Part 2

let ans2 = data

ans2