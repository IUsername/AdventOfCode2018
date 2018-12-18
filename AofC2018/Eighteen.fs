module Eighteen

open System.Collections.Generic

type Point = (int*int) 

type Acre = | Open | Trees | Lumberyard
    with static member ToCh (a:Acre) =
            match a with | Open->'.' | Lumberyard->'#' | Trees->'|'            

let private nearby (x:int,y:int) = 
    seq {
        for dx in [-1..1] do
            for dy in [-1..1] do
                if not (dx = 0 && dy = 0) then yield (x+dx,y+dy)            
    }

type XYGrid<'T>(min:Point, max:Point, init:'T) = 
    let (minX,minY) = min
    let (maxX,maxY) = max
    let h = maxY - minY + 1
    let w = maxX - minX + 1    
    let array = Array2D.create h w init
    member __.Item
        with get ((x,y)) = array.[y - minY, x - minX]
        and  set ((x,y)) (value: 'T) = array.[y - minY, x - minX] <- value  
    member __.Width = w
    member __.Height = h
    member this.CellSeqP = 
        seq {
            for y in [0..this.Height-1] do
                for x in [0..this.Width-1] do
                    let p = (x+fst min,y+snd min)
                    yield (p, this.[p])
        }
    member __.InBounds (p:Point) =
        let (x,y) = p
        x >= minX && x <= maxX && y >= minY && y <= maxY   
        
let genHash (area:XYGrid<Acre>) =
    area.CellSeqP |> Seq.map (fun e -> (hash e)) |> Seq.fold (fun s c -> (s*23)+c) 17

let hasAtLeast (count:int) (key:'T) (dict:IDictionary<'T,int>) =
    match dict.TryGetValue key with
    | true,value -> value >= count
    | false,_ -> false

let updateAcre (area:XYGrid<Acre>) (p:Point) =
    let a = area.[p]
    let around = p |> nearby |> Seq.filter (fun p' -> area.InBounds p') |> Seq.map (fun p' -> area.[p']) |> Seq.countBy id |> dict
    match a with
    | Open -> if around |> hasAtLeast 3 Trees then Trees else Open
    | Trees -> if around |> hasAtLeast 3 Lumberyard then Lumberyard else Trees
    | Lumberyard -> if around |> hasAtLeast 1 Lumberyard && around |> hasAtLeast 1 Trees then Lumberyard else Open

let update (area:XYGrid<Acre>) (min:int) =
    let rec loop (max:int) (r:int) (area':XYGrid<Acre>) (hashes:Map<int,int>) (found:bool) = 
        if r = max then area' 
        else           
            let current = area'.CellSeqP 
            let area'' = XYGrid<Acre>((0,0),(49,49), Open)
            current |> Seq.map (fun (p,_) -> (p, updateAcre area' p)) |> Seq.iter (fun (p,a') -> area''.[p] <- a')
            let r' = r+1
            if found then loop max r' area'' hashes true
            else    
                let hash = area'' |> genHash
                if hashes.ContainsKey hash then
                    let prior = hashes.[hash]
                    let cycle = r' - prior
                    let remains = max - r'
                    let max' = remains % cycle
                    loop max' 0 area'' hashes true
                else 
                    let hashes'= hashes.Add (hash,r')
                    loop max r' area'' hashes' false
    loop min 0 area Map.empty false

let private concatMap f (cells:seq<_>) =
    cells |> Seq.map f |> String.concat ""

let private printMap (area:XYGrid<Acre>) =
    area.CellSeqP
    |> Seq.map snd 
    |> Seq.splitInto area.Height 
    |> Seq.map (concatMap (Acre.ToCh >> (fun c -> c.ToString())))
    |> Seq.iter (printfn "%s") 

let private parseLine (l:string) (y:int) =
    l.ToCharArray() 
    |> Array.mapi (fun x ch ->
        match ch with
        | '.' -> ((x,y),Open)
        | '#' -> ((x,y),Lumberyard)
        | '|' -> ((x,y),Trees)
        |  _  -> failwith "unknown char") 

let execute = fun d -> 
    let data = d |> Parsing.splitLines |> Seq.mapi (fun y l -> parseLine l y) |> Seq.concat |> List.ofSeq
    let area = XYGrid<Acre>((0,0),(49,49), Open)
    data |> Seq.iter (fun (p,a) -> area.[p] <- a)
    let area' = update area 10
    let results = area'.CellSeqP |> Seq.map(fun (_,a) -> a) |> Seq.countBy id |> dict
    printfn "Day 18 - Part 1: Value of %i" (results.[Trees]*results.[Lumberyard])

    let area = XYGrid<Acre>((0,0),(49,49), Open)
    data |> Seq.iter (fun (p,a) -> area.[p] <- a)
    let area' = update area 1000000000
    let results = area'.CellSeqP |> Seq.map(fun (_,a) -> a) |> Seq.countBy id |> dict
    printfn "Day 18 - Part 2: Value of %i" (results.[Trees]*results.[Lumberyard])
    //printMap area


let dataSet = @"
..##|#.##..|#...|.#|....#|.#......|#......#|....|.
..#|.#.#|..#.|...|.|.|....|..|||||..#|..#.#..|##.|
.||.....#..#.....|||#|.....#|###|||.|..#...#..|##.
#|...|#|.......##.|......####.|..||#....##||.#...#
..|.##|#.|.#||#....#||...|#.||.|....|.|#|.#...#.#.
#..|......#..#....|||.||..#..#..#.|.|.|#.||.....#.
|.|...#|..|#|.|....#...#.|.#||.....#........||..|.
.#|##.|...|......|.#||#|#..|.|....|....|||...#####
.|.......#....##|.#.#...|.||.....#|.|#.......|##|.
.#....#|##|..##|..#.|...##.|#.##..#.......||.|.|##
###..#||........#...#..#..|......||.......#.|#|#..
.||.#.....|.#...|......#.||##||......|...||.||....
..#|.|....#.#.|||#...#.....#.#.#.|....#.|...|#....
#...|..#.|.|...#|..#.|#...|.......#.|.......|.###.
.|#|.#.|..#|....|..|..#..#|......#..#..|.#...|.|#.
.#...#......#|||..|.|.....#....|#.||.....#||##..|.
|.|...#||..|........#.....|#....|...||..##.#.#.|..
.....|......######|...|.....##.........|#|.#|.....
|..|.......|#|.##.|..|....#....##..||..|...|..|...
.||||#....|..|.|#|..|...#.|#.|.....|.||.||#...|...
.#|#..###.#|....#..||...|##..#.#|..#..|||........#
..|.#.....#|..|.#..|...#||......##.|....|.|#.|.|||
..#.......|#||..|...|.....##..#.#.####..|......#|#
.|##......|#....|..|.||...##|#....##||#.#|#.#..#.|
#..#..|..#....|..|....##..|..#....##.#|#|##|#|....
|####..#....|..|..|....|#.|....|.....##.##.#|....|
..||...##..|...#|##..|.##......#...##.|....#.|...#
.#...|#.|#|.....|#|....##.|.........|.......|.#...
||...###...|#..###|..|.#.|#||...#...#|.....|##|..|
#.#.#|....#|#..|..........|#..#|.|#||...|##.##.|#.
....|.##..#...#..##|..|....|..||#.|..|..#..#......
.|.#..|.#...||..#..|.|...#....|.||#.|#.....||.|...
..#|||.#..|#|...||#.|....|.#...#||||#...#...|...|#
..#..#....#|.............##...|..#..#..||##|..#.||
#....#|...#..##....###..||..#||...|.#..|.....|....
....|..#...#...||..||....|#|#|.|..|.#.|..|.##..|#.
..#.....|....||.##..#..#|..|.|#.....|...|..|..#..#
.##.||.#||..#|.#....||.|.....#|.....#....||..#.##|
..|.#|.|...|........#......|.##.|#.#..|......##...
.##||.|.##....|...##.#.....#.##.##..#...|||#|#.|.|
....|||..|....#..#.#..|.|.|....#.|#.#.##|.|#.#|.#.
..|...#|#....##.#|##.#.||##...#.|#..##.....#...#..
.|#..#.....|...|.#..##......|..#.|.......#.....#..
.#..|.#..|#...#....|..||.|..#..#...##........#....
.|.##.#|.#.#.|..||##|..||||.##|||..#..##...|..#|#.
#.......#...|#.|#||..|.##...#...|....|...##....#.|
.###..|......||#...|..||||#....|.||...#....|.#...|
.|.#...|#..|.....#......|.......|.........|.#.#...
|.|...#...|#|||...|||....|#..|#...#.#..#...|....#|
|#...#..#.|#|.#..#.#.....|.|.##...#.|#..|.#|..#...
"