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

let rotate (axis, pos, deg') (x,y,z) =
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

let rotateRev (axis,pos,deg') coord = rotate (axis, pos, (360-deg')) coord

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

let findLoc2 p1 p2 =
    seq {
        for (i1,i) in allTransforms p1 do
            for (i2,j) in allTransforms p2 do
                yield (i1,i2,i,j,Array.allPairs i j
                                 |> Array.map sub
                                 |> Array.countBy id
                                 |> Array.maxBy snd)
    }
    |> Array.ofSeq

let findVec p1 p2 =
    let (i1,i2,i,j,(loc,_)) =
        findLoc2 p1 p2
        |> Array.filter (fun (i1,i2,_,_,(loc,cnt)) -> cnt >= 12)
        |> Array.head
        
    let rotateToSource = rotate i2 >> rotateRev i1

    rotateToSource,
     rotateRev i1 loc (* in source orientation*),
     p2 |> Array.map (rotateToSource) (* in source orientation*)
    
let loc0 = (0,0,0)
let (s1to0, loc1, bec1') = findVec example[0] example[1]
let bec1 = bec1' |> Array.map (fun c -> add(loc1, c))
let (s4to1, loc4', bec4') = findVec example[1] example[4]
let (s3to1, loc3', bec3') = findVec example[1] example[3]
let (s2to4, loc2', bec2') = findVec example[4] example[2]

let loc4 = add (loc1,s1to0 loc4')
let bec4 = bec4' |> Array.map (fun b -> add(loc4, (s1to0) b))

let loc3 = add (loc1,s1to0 loc3')
let bec3 = bec3' |> Array.map (fun b -> add(loc3, (s1to0) b))

let loc2 = add (loc4,(s4to1 >> s1to0) loc2')
let bec2 = bec2' |> Array.map (fun b -> add(loc2, (s4to1 >> s1to0) b))

Array.concat [|
    example[0]; bec1; bec2; bec3; bec4
|]
|> Array.distinct
|> Array.length

type Rotator = (int*int*int) -> (int*int*int)

let solve data (pairMap : Map<int,int array>) =
    let rec f visited (locs : Map<int,int*int*int>) (rots : Map<int,Rotator>) beacons =
        if (Set.count visited = Array.length data) then
            locs, beacons
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

            printfn "Source,target = (%i,%i)" source target
            let (rotToSource, loc', bec) = findVec data[source] data[target]

            let rotTo0 = rotToSource >> rots[source]
            
            let loc = rots[source] loc' |> (fun l -> add(locs[source], l))
            let becsIn0 = bec |> Array.map rots[source] |> Array.map (fun b -> add(loc,b))

            printfn "Loc %i: %A" target loc
            
            let newBecs = Set.union beacons (Set.ofArray becsIn0)
            let newLoc = Map.add target loc locs
            let newRots = Map.add target rotTo0 rots
            let newVisited = Set.add target visited
            f newVisited newLoc newRots newBecs

    let initVisited = set [0]
    let initLocs = Map.ofList [0,(0,0,0)]
    let initRots = Map.ofList [0,id]
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

let pairMap =
    allPairsData
    |> Array.collect (fun (x,y,_) -> [|(x,y);(y,x)|])
    |> Array.groupBy fst
    |> Array.map (fun (c,arr) -> c,arr |> Array.map snd)
    |> Map.ofArray

// 605 too high

let (locs,solved) = solve data pairMap

let ans1 = solved |> Set.count

ans1

/// Part 2

let man (x,y,z) (x2,y2,z2) = abs (x-x2) + abs (y-y2) + abs (z-z2)

let ans2 =
    let arr = 
        Map.values locs
        |> Array.ofSeq
    
    Array.allPairs arr arr
    |> Array.map (fun (a,b) -> man a b)
    |> Array.max

ans2