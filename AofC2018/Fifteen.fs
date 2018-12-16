module Fifteen

open System.Collections.Generic

type Point = (int*int)

type CaveCell = | Wall | Open

type Creature = | Elf | Goblin

type CreatureInfo = {kind:Creature; hp:int}

type CreatureCell = | Occupied of CreatureInfo | Unoccupied

type Weight = | Blocked | Weight of weight:int

type Node = {p:Point;weight:Weight}

type Lookup = Point -> Point -> Node option

type Traveral =
    {
        from: Dictionary<Node,Node>
        costs: Dictionary<Node,int>
        path: Node list
        first: Point
        total: int
    }           
    static member empty =
        {
            from = Dictionary<Node,Node>()
            costs = Dictionary<Node,int>()
            path = []
            first = (0,0)
            total = 0
        }

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

let cost n = match n.weight with | Weight w -> w | Blocked -> failwith "no cost with blocked node"

let manhattan (pA:Point) (pB:Point) = abs (fst pA - fst pB) + abs (snd pA - snd pB)

let offset (delta:Point) (p:Point) = ((fst p + fst delta),(snd p + snd delta))

let neighbors (l:Lookup) node goal =
    cross 
        |> List.map (fun delta -> node.p |> offset delta |> l goal)
        |> List.choose id  
        
let buildPath t start goal =
    let mutable current = goal
    let mutable total = cost goal
    let mutable path = [current]
    while current <> start do
        current <- t.from.[current]
        path <- current::path
        total <- total + cost current
    let first = path |> List.skip 1 |> List.head
    {t with path=path; total=total;first=first.p}

let sortByCostAndReadOrder = fun (node,cost) -> 
    let (x,y) = node.p
    cost,y,x

let aStar (l:Lookup) start goal =
    let mutable (frontier:(Node*int) list) = []
    let enqueue value = frontier <- value::frontier
    let dequeue () =
        let sorted = frontier |> List.sortBy sortByCostAndReadOrder
        frontier <- sorted.Tail
        sorted.Head

    enqueue (start,cost start)

    let t = Traveral.empty
    t.from.[start] <- start
    t.costs.[start] <- cost start

    let mutable foundGoal = false
    while frontier.Length > 0 && not(foundGoal) do
        let current,_ = dequeue()
        if current = goal then foundGoal <- true
        if not(foundGoal) then
            neighbors l current goal.p
            |> List.iter (fun n ->
                let cost' = t.costs.[current] + cost n
                if (not(t.costs.ContainsKey n) || cost' < t.costs.[n]) then   
                    t.costs.[n] <- cost'
                    let priority = cost'// + manhattan n.p goal.p
                    enqueue (n,priority)
                    t.from.[n] <- current)
    if foundGoal then
        Some (buildPath t start goal)
    else None

let zipGrids (a:XYGrid<_>) (b:XYGrid<_>) = 
    a.CellSeqP |> Seq.zip b.CellSeqP |> Seq.map (fun ((p,ac),(_,bc)) -> (p,(ac,bc)))

let createLookup (cave:XYGrid<CaveCell>) (creatures:XYGrid<CreatureCell>) =
    (fun g p ->
        match cave.[p] with
        | Wall -> None
        | Open -> match creatures.[p] with
                    | Unoccupied -> Some {p=p;weight=(Weight 1)}
                    | Occupied _ when p=g -> Some {p=p;weight=(Weight 1)}
                    | _          -> None)

let toNode (p:Point) = {p=p;weight=(Weight 1)}

let private neighborEnemies (creatures:XYGrid<CreatureCell>) (p:Point,i:CreatureInfo) =
    let creatureFilter = fun kind (p,cell) ->
        match cell with
        | Unoccupied -> None
        | Occupied ci when ci.kind<>kind -> Some (p,ci)
        | _ -> None
    let near = cross |> List.map (fun delta -> p |> offset delta |> (fun p' -> (p',creatures.[p'])))
    near |> List.map (creatureFilter i.kind) |> List.choose id

let private findCreatures (creatures:XYGrid<CreatureCell>) =
    creatures.CellSeqP |> Seq.map (fun (p,c) -> 
        match c with
        | Unoccupied -> None
        | Occupied i -> Some (p,i))

let findEnemies (p:Point,i:CreatureInfo) (c:seq<Point*CreatureInfo>) =
    c |> Seq.filter (fun (p',ci) -> ci.kind <> i.kind && p <> p')
        
let private charToCaveCell (ch:char) =
    match ch with | '#' -> Wall | _ -> Open

let private charToCreatureCell (hp:int) (ch:char) =
    match ch with
    | 'G' -> Occupied {kind=Goblin;hp=hp}
    | 'E' -> Occupied {kind=Elf;hp=hp}
    |  _  -> Unoccupied

let private charToCaveCreatures (hp:int) (p:Point) (ch:char) =
    (p, (charToCaveCell ch), (charToCreatureCell hp ch))

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

let private checkConflict (creatures:XYGrid<CreatureCell>) =
    let all = creatures |> findCreatures |> Seq.choose id |> List.ofSeq
    all |> List.countBy (fun (_,ci) -> ci.kind) |> List.length > 1

let private fight (creatures:XYGrid<CreatureCell>) (p:Point,i:CreatureInfo) =
    let near = neighborEnemies creatures (p,i)
    let weakest = near |> List.sortBy (fun ((x,y),ci) -> ci.hp,y,x) |> List.tryHead
    match weakest with
    | None -> true
    | Some (p,ci) -> 
        let hp' = ci.hp - 3
        if hp' < 1 then
            creatures.[p] <- Unoccupied
            checkConflict creatures
        else 
            creatures.[p] <- Occupied {ci with hp=hp'}
            true

let private move (l:Lookup) (creatures:XYGrid<CreatureCell>) (cr:(Point*CreatureInfo)) =
    let cc' = creatures.[fst cr]
    match cc' with
    | Unoccupied -> true
    | Occupied _ ->
        let all = creatures |> findCreatures |> Seq.choose id
        let enemies = all |> findEnemies cr
        let path = enemies 
                    |> Seq.map (fun e -> aStar l (toNode (fst cr)) (toNode (fst e))) 
                    |> Seq.choose id                 
                    |> Seq.sortBy (fun t -> 
                        let target = t.path |> List.last
                        let (x,y) = target.p
                        t.total,y,x)                   
                    |> Seq.tryHead    
        match path with
        | None -> true
        | Some t when t.path.Length < 3 -> // Cannot move on top of goal
            fight creatures cr 
        | Some t ->       
            let p = fst cr
            creatures.[p] <- Unoccupied
            creatures.[t.first] <- Occupied (snd cr)
            fight creatures (t.first, snd cr) 

let private moveAll (l:Lookup) (creatures:XYGrid<CreatureCell>) =
    let all = creatures |> findCreatures |> Seq.choose id |> List.ofSeq
    let results = all |> List.map (fun cr -> move l creatures cr) 
    let endFighting = not(results |> List.forall id)
    if endFighting then
        let completedRound = results |> List.findIndex (fun r -> r = false) = results.Length - 1
        (true, completedRound)
    else
        (false, true)
    
let private outcome (round:int) (creatures:XYGrid<CreatureCell>) =
     let all = creatures |> findCreatures |> Seq.choose id |> List.ofSeq
     let sum = all |> List.sumBy (fun (_,ci) -> ci.hp)
     sum * round

let private printMap (width:int) cave creatures  =
    zipGrids cave creatures |> Seq.map snd |> Seq.splitInto width |> Seq.map (concatMap infoToStr) |> Seq.iter (printfn "%s") 

let execute = fun d ->     
    let lines = d |> Parsing.splitLines
    let N = lines |> Seq.length
    let M = lines |> Seq.head |> (fun l -> l.Length)
    let cave = XYGrid<CaveCell>(N,M,Open)
    let creatures = XYGrid<CreatureCell>(N,M,Unoccupied)
    let parsed = lines |> Seq.indexed |> Seq.map (mapLine (charToCaveCreatures 200)) |> Seq.concat
    parsed |> Seq.iter (fun (p,cc,crc) -> cave.[p] <- cc; creatures.[p] <- crc)
    printMap N cave creatures 

    let mutable round = 0
    let mutable fighting = true
    let l = createLookup cave creatures
    while fighting do   
        let (endMatch,completedRound) = moveAll l creatures           
        if completedRound then
            round <- round + 1 
            printfn ""
            printfn "Round %i" round
            printMap N cave creatures
            let all = creatures |> findCreatures |> Seq.choose id |> List.ofSeq |> List.map (fun (_,ci) -> ci.hp)
            printfn "%A" all        
        fighting <- not(endMatch)
    
    let outcome = creatures |> outcome round
    printfn "Outcome %i after round %i" outcome round
    printMap N cave creatures 
    let all = creatures |> findCreatures |> Seq.choose id |> List.ofSeq |> List.map (fun (_,ci) -> ci.hp)
    printfn "%A" all

let testSet1 = @"
#########
#G..G..G#
#.......#
#.......#
#G..E..G#
#.......#
#.......#
#G..G..G#
#########
"

let testSet2 = @"
#######   
#.G...#   
#...EG#   
#.#.#G#   
#..G#E#   
#.....#   
#######
"
let testSet3 = @"
#######
#G..#E#
#E#E.E#
#G.##.#
#...#E#
#...E.#
#######
"

let testSet4 = @"
####### 
#E..EG# 
#.#G.E# 
#E.##E# 
#G..#.# 
#..E#.# 
####### 
"

let testSet5 = @"
#######
#E.G#.#
#.#G..#
#G.#.G#
#G..#.#
#...E.#
#######
"

let testSet6 = @"
#######
#.E...#
#.#..G#
#.###.#
#E#G#G#
#...#G#
#######
"

let testSet7 = @"
#########
#G......#
#.E.#...#
#..##..G#
#...##..#
#...#...#
#.G...G.#
#.....G.#
#########
"

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

let dataSet2 = @"
################################
#######.G...####################
#########...####################
#########.G.####################
#########.######################
#########.######################
#########G######################
#########.#...##################
#########.....#..###############
########...G....###.....########
#######............G....########
#######G....G.....G....#########
######..G.....#####..G...#######
######...G...#######......######
#####.......#########....G..E###
#####.####..#########G...#....##
####..####..#########..G....E..#
#####.####G.#########...E...E.##
#########.E.#########.........##
#####........#######.E........##
######........#####...##...#..##
###...................####.##.##
###.............#########..#####
#G#.#.....E.....#########..#####
#...#...#......##########.######
#.G............#########.E#E####
#..............##########...####
##..#..........##########.E#####
#..#G..G......###########.######
#.G.#..........#################
#...#..#.......#################
################################
"