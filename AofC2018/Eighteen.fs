module Eighteen

open System.Collections.Generic

type Point = (int*int) 

type Acre = | Open | Trees | Lumberyard
    with static member ToCh (a:Acre) =
            match a with | Open->'.' | Lumberyard->'#' | Trees->'|'

let S (p:Point) = (fst p, snd p+1)
let W (p:Point) = (fst p-1, snd p)
let E (p:Point) = (fst p+1, snd p)
let N (p:Point) = (fst p, snd p-1)
let NW (p:Point) = (fst p-1, snd p-1)
let NE (p:Point) = (fst p+1, snd p-1)
let SE (p:Point) = (fst p+1, snd p+1)
let SW (p:Point) = (fst p-1, snd p+1)
let allDir (p:Point) = [(N p);(NE p);(E p);(SE p);(S p);(SW p);(W p);(NW p)]

type XYGrid<'T>(min:Point, max:Point, init:'T) = 
    let h = snd max - snd min + 1
    let w = fst max - fst min + 1    
    let array = Array2D.create h w init
    member __.Item
        with get (p:Point) = array.[snd p - snd min, fst p - fst min]
        and  set (p:Point) (value: 'T) = array.[snd p - snd min, fst p - fst min] <- value  
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
        x >= fst min && x <= fst max && y >= snd min && y <= snd max       

let hasAtLeast (count:int) (key:'T) (dict:IDictionary<'T,int>) =
    match dict.TryGetValue key with
    | true,value -> value >= count
    | false,_ -> false

let updateAcre (area:XYGrid<Acre>) (p:Point) =
    let a = area.[p]
    let around = p |> allDir |> List.filter (fun p' -> area.InBounds p') |> List.map (fun p' -> area.[p']) |> List.countBy id |> dict
    match a with
    | Open -> if around |> hasAtLeast 3 Trees then Trees else Open
    | Trees -> if around |> hasAtLeast 3 Lumberyard then Lumberyard else Trees
    | Lumberyard -> if around |> hasAtLeast 1 Lumberyard && around |> hasAtLeast 1 Trees then Lumberyard else Open

let update (area:XYGrid<Acre>) (min:int) =
    let rec loop (r:int) (area':XYGrid<Acre>) = 
        if r = 0 then area' 
        else
            let current = area'.CellSeqP |> List.ofSeq
            let area'' = XYGrid<Acre>((0,0),(49,49), Open)
            current |> List.map (fun (p,_) -> (p, updateAcre area' p)) |> List.iter (fun (p,a') -> area''.[p] <- a')
            loop (r-1) area''
    loop min area

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
    let data = d |> Parsing.splitLines |> Seq.mapi (fun y l -> parseLine l y) |> Seq.concat
    let area = XYGrid<Acre>((0,0),(49,49), Open)
    data |> Seq.iter (fun (p,a) -> area.[p] <- a)
    let area' = update area 10
    let results = area'.CellSeqP |> Seq.map(fun (_,a) -> a) |> Seq.countBy id |> dict
    printfn "%i" (results.[Trees]*results.[Lumberyard])
    printMap area


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