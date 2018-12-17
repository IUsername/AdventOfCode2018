module Fifteen

open System.Collections.Generic

type Point = (int*int)

type CaveCell = | Wall | Open

type Creature = | Elf | Goblin

type CreatureInfo = {guid:int; kind:Creature; hp:int;}

type CreatureState = (Point*CreatureInfo)

type CreatureCell = | Occupied of CreatureInfo | Unoccupied

type Lookup = Point -> (Point*CreatureInfo) option

type IsOpen = Point -> bool

type XYGrid<'T>(N:int, M:int, init:'T) = 
    let array = Array2D.create N M init
    member __.Item
        with get (p:Point) = array.[snd p, fst p]
        and  set (p:Point) (value: 'T) = array.[snd p, fst p] <- value  
    member this.CellSeqP = 
        seq {
            for y in [0..N-1] do
                for x in [0..N-1] do
                    let p = (x,y)
                    yield (p, this.[p])
        }

let cross = [(0,-1);(-1,0);(1,0);(0,1)] 

let manhattan (pA:Point) (pB:Point) = abs (fst pA - fst pB) + abs (snd pA - snd pB)

let isAdjacent (pA:Point) (pB:Point) = (manhattan pA pB) = 1

let offset (delta:Point) (p:Point) = ((fst p + fst delta),(snd p + snd delta))

let neighbors (l:Lookup) (p:Point) =
    cross |> List.map (fun d -> offset d p |> l) |> List.choose id  

let zipGrids (a:XYGrid<_>) (b:XYGrid<_>) = 
    a.CellSeqP |> Seq.zip b.CellSeqP |> Seq.map (fun ((p,ac),(_,bc)) -> (p,(ac,bc)))

let createLookup (creatures:XYGrid<CreatureCell>) : Lookup =
    (fun p -> match creatures.[p] with
                | Unoccupied -> None
                | Occupied o -> Some (p,o))

let createIsOpen (cave:XYGrid<CaveCell>) (creatures:XYGrid<CreatureCell>) : IsOpen =
    (fun p -> match cave.[p] with
                | Wall -> false
                | Open -> match creatures.[p] with
                            | Unoccupied -> true
                            | Occupied _ -> false)

let private findCreatures (creatures:XYGrid<CreatureCell>) =
    creatures.CellSeqP 
        |> Seq.map (fun (p,c) -> 
            match c with
            | Unoccupied -> None
            | Occupied i -> Some (p,i)) 
        |> Seq.choose id

let findEnemies (i:CreatureInfo) (c:seq<Point*CreatureInfo>) =
    c |> Seq.filter (fun (_,ci) -> ci.kind <> i.kind)
        
let private charToCaveCell (ch:char) =
    match ch with | '#' -> Wall | _ -> Open

let private charToCreatureCell (identifier:(unit->int)) (hp:int) (ch:char)  =
    match ch with
    | 'G' -> Occupied {guid=identifier();kind=Goblin;hp=hp}
    | 'E' -> Occupied {guid=identifier();kind=Elf;hp=hp}
    |  _  -> Unoccupied

let private charToCaveCreatures (identifier:(unit->int)) (hp:int) (p:Point) (ch:char) =
    (p, (charToCaveCell ch), (charToCreatureCell identifier hp ch))

let private caveCellToStr (cell:CaveCell) =
    match cell with | Wall -> "#" | Open -> "."

let private creatureCellToStr (cell:CreatureCell) =
    match cell with 
    | Occupied c when c.kind=Elf -> "E" 
    | Occupied c when c.kind=Goblin -> "G" 
    | _ -> " "

let private concatMap f (cells:seq<_>) =
    cells |> Seq.map f |> String.concat "" 

let private infoToStr (crc:CreatureCell,cc:CaveCell) =
    match crc with
    | Unoccupied -> caveCellToStr cc
    | _ -> creatureCellToStr crc    

let private mapLine f (y:int,l:string) =
    l.ToCharArray() |> Array.toSeq |> Seq.mapi (fun x ch -> f (x,y) ch)

let private fightCreature elfAttack (creatures:XYGrid<CreatureCell>) (p:Point,ci:CreatureInfo) =
    let isElf = ci.kind = Elf
    let attack = if not isElf then elfAttack else 3
    let hp' = ci.hp - attack
    if hp' < 1 then creatures.[p] <- Unoccupied; isElf
    else creatures.[p] <- Occupied {ci with hp=hp'}; false
    
let private takeStep enemies creatures cave creature =
    let isOpenFun = createIsOpen cave creatures     
    let (location,ci) = creature
    let queue = Queue<Point>()
    let from = Dictionary<Point,Point>()
    queue.Enqueue(location)
    from.Add (location,(-1,-1))

    while queue.Count > 0 do    
        let p' = queue.Dequeue()
        cross |> Seq.map (offset p') 
              |> Seq.filter isOpenFun 
              |> Seq.filter (from.ContainsKey >> not)
              |> Seq.iter (fun n -> queue.Enqueue(n); from.Add(n,p'))

    let getPath = (fun p ->
        if not(from.ContainsKey(p)) then None 
        else let mutable current = p
             let mutable path = []
             while current <> location do
                 path <- current::path
                 current <- from.[current]
             Some path)

    let inRange = enemies 
                |> Seq.map (fun (p,_) -> cross |> Seq.map (offset p)) 
                |> Seq.concat 
                |> Seq.distinct
                |> Seq.filter isOpenFun   
                |> List.ofSeq

    let bestPath = inRange 
                |> Seq.map (fun t -> (t, getPath t)) 
                |> Seq.choose (fun (t,po) -> match po with | Some p -> Some (t,p) | _ -> None)
                |> Seq.sortBy (fun ((x,y),p) -> p.Length, y, x)
                |> Seq.map snd
                |> Seq.tryHead

    match bestPath with     
    | None      -> location
    | Some path -> let firstStep = path |> List.head
                   creatures.[location]  <- Unoccupied
                   creatures.[firstStep] <- Occupied ci 
                   firstStep

let private tryFindCreature creatures guid =
    creatures |> findCreatures |> Seq.tryFind (fun (_,ci) -> ci.guid=guid)

let private move elfAttack creatures cave guid =    
    let info = guid |> tryFindCreature creatures   
    match info with
    | None -> (true,false)
    | Some (l,ci) ->
        let mutable location = l
        let enemies = creatures |> findCreatures |> findEnemies ci |> List.ofSeq
        if enemies |> List.isEmpty then (false,false)
        else
            let adjacent = enemies |> Seq.filter (fun (p,_) -> isAdjacent location p) 

            if adjacent |> Seq.isEmpty then 
                location <- takeStep enemies creatures cave (l,ci)

            let bestAdjacent = enemies 
                                |> Seq.filter (fun (p,_) -> isAdjacent location p) 
                                |> Seq.sortBy (fun ((x,y),i) -> i.hp,y,x) 
                                |> Seq.tryHead

            match bestAdjacent with
            | Some eci -> (true,fightCreature elfAttack creatures eci)
            | None -> (true,false)    

let private moveAll stopOnElfDeath elfAttack creatures cave =
    let mutable continueFight = true
    let mutable elfDied = false;
    creatures 
        |> findCreatures 
        |> List.ofSeq
        |> Seq.map (fun (_,ci) -> ci.guid) 
        |> Seq.takeWhile (fun _ -> continueFight && not(stopOnElfDeath && elfDied) ) 
        |> Seq.iter (fun cr -> 
            let (c,e) = move elfAttack creatures cave cr
            continueFight <- c
            elfDied <- e) 
    (continueFight,elfDied)    
    
let private outcome round (creatures:XYGrid<CreatureCell>) =
     round * (creatures |> findCreatures |> Seq.sumBy (fun (_,ci) -> ci.hp))

let createIdentifier =
    let mutable current = 0
    (fun () -> current <- current + 1; current)

let private printMap width cave creatures  =
    zipGrids cave creatures 
    |> Seq.map snd 
    |> Seq.splitInto width 
    |> Seq.map (concatMap infoToStr) 
    |> Seq.iter (printfn "%s") 

let execute = fun d ->     
    let lines = d |> Parsing.splitLines
    let N = lines |> Seq.length
    let M = lines |> Seq.head |> (fun l -> l.Length)
    let cave = XYGrid<CaveCell>(N,M,Open)
    let creatures = XYGrid<CreatureCell>(N,M,Unoccupied)
    let identifier = createIdentifier
    let parsed = lines 
                    |> Seq.indexed 
                    |> Seq.map (mapLine (charToCaveCreatures identifier 200)) 
                    |> Seq.concat 
                    |> List.ofSeq
    parsed |> Seq.iter (fun (p,cc,crc) -> cave.[p] <- cc; creatures.[p] <- crc)

    let mutable round = 0
    let mutable fighting = true
    let mutable elfDied = false;
    while fighting do   
        let (f,e) = moveAll false 3 creatures cave           
        fighting <- f
        elfDied <- e
        if fighting then
            round <- round + 1 
            //printfn ""
            //printfn "Round %i" round
            //printMap N cave creatures     
    
    let outcome1 = creatures |> outcome round
    printfn "Day 14 - part 1: Outcome %i after round %i" outcome1 round
    //printMap N cave creatures 

    elfDied <- true  
    let mutable elfAttack = 4
    let mutable outcome2 = 0

    while elfDied do
        let creatures' = XYGrid<CreatureCell>(N,M,Unoccupied)
        parsed |> Seq.iter (fun (p,_,crc) -> creatures'.[p] <- crc)

        round <- 0
        fighting <- true
        elfDied <- false        
        
        while fighting do   
            let (f,e) = moveAll true elfAttack creatures' cave           
            fighting <- f
            elfDied <- e
            if fighting then
                round <- round + 1 
            if elfDied then
                elfAttack <- elfAttack + 1
                fighting <- false

        if not elfDied then
            outcome2 <- creatures' |> outcome round

    printfn "Day 14 - part 2: Outcome %i with attack of %i" outcome2 elfAttack

let dataSet = @"
################################
####.#######..G..########.....##
##...........G#..#######.......#
#...#...G.....#######..#......##
########.......######..##.E...##
########......G..####..###....##
#...###.#.....##..G##.....#...##
##....#.G#....####..##........##
##..#....#..#######...........##
#####...G.G..#######...G......##
#########.GG..G####...###......#
#########.G....EG.....###.....##
########......#####...##########
#########....#######..##########
#########G..#########.##########
#########...#########.##########
######...G..#########.##########
#G###......G#########.##########
#.##.....G..#########..#########
#............#######...#########
#...#.........#####....#########
#####.G..................#######
####.....................#######
####.........E..........########
#####..........E....E....#######
####....#.......#...#....#######
####.......##.....E.#E...#######
#####..E...####.......##########
########....###.E..E############
#########.....##################
#############.##################
################################
"