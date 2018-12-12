module Eleven

open FSharp.Collections.ParallelSeq

let hundreds (value:int) = 
    value / 100 % 10

type Coord = {x:int; y:int} with
    member this.Offset (dx:int, dy:int) =
        {x=this.x+dx; y=this.y+dy}
    member this.Shift = {x=this.x-1; y=this.y-1}
    member this.ToTuple = (this.x,this.y)

let powerLevel (c:Coord) (sn:int) = 
    let rackId = c.x + 10
    ((((c.y * rackId) + sn) * rackId) |> hundreds) - 5    

type FuelCell = {c:Coord; pl:int}

let toFuelCell (sn:int) = fun i j ->
    let c = {Coord.x=i+1; y=j+1}
    {FuelCell.c=c; pl=(powerLevel c sn)}

type Grid (N:int, sn:int) = 
    let internalArray = Array2D.init N N (toFuelCell sn) 

    // Summed-area table
    let sat = Array2D.create N N 0

    member __.Item
        with get (c:Coord) = internalArray.[c.x - 1,c.y - 1]
        and  set (c:Coord) (value: FuelCell) = internalArray.[c.x - 1,c.y - 1] <- value

    member private __.xySAT (x:int, y:int) =
        if x < 0 || y < 0 then 0 else sat.[x,y]

    member private __.GetSAT (c:Coord) =
        let (x,y) = c.Shift.ToTuple
        if x < 0 || y < 0 then 0 else sat.[x,y]

    member this.BuildSAT =         
        for y in [0..N-1] do
            for x in [0..N-1] do                  
                let i = this.xySAT (x-1,y-1)
                let j = this.xySAT (x,y-1)
                let k = this.xySAT (x-1,y)
                let fc = internalArray.[x,y].pl                   
                sat.[x,y] <- fc + j + k - i    
                
    member private this.AreaSum (size:int) (coord:Coord) =   
        let delta = size-1
        let a = this.GetSAT (coord.Offset(-1,-1))
        let b = this.GetSAT (coord.Offset(delta, -1))
        let c = this.GetSAT (coord.Offset(-1, delta))
        let d = this.GetSAT (coord.Offset(delta,delta))
        (d + a - b - c)

    member __.AllCells (size:int) = 
        seq {
            for y in [0..N-size] do
                for x in [0..N-size] do
                    yield internalArray.[x,y]
        }

    member this.Blocks (size:int) = 
        this.AllCells size 
        |> Seq.map (fun fc -> (fc.c, this.AreaSum size fc.c))  

let sizeToMax (g:Grid) (s:int) =
    g.Blocks s |> Seq.maxBy (fun (_,pl) -> pl)

let execute = fun d ->    
    let g = Grid (300,d)    
    g.BuildSAT

    let (c,_) = 3 |> sizeToMax g
    printfn "Day 11 - part 1: Largest power coordingate is %i,%i" (c.x) (c.y)

    let (s,(c,_)) = 
        [1..300] 
        |> PSeq.withDegreeOfParallelism 10 
        |> PSeq.map (fun s -> (s, s |> sizeToMax g)) 
        |> Seq.maxBy (fun (_,(_,p)) -> p)

    printfn "Day 11 - part 2: Largest power coordingate is %i,%i,%i" (c.x) (c.y) s



