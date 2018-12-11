module Eleven

open FSharp.Collections.ParallelSeq

let hundreds (value:int) = 
    value / 100 % 10

type Coord = {x:int; y:int} with
    member this.Offset (dx:int, dy:int) =
        {x=this.x+dx; y=this.y+dy}

let powerLevel (c:Coord) (sn:int) = 
    let rackId = c.x + 10
    ((((c.y * rackId) + sn) * rackId) |> hundreds) - 5    

type FuelCell = {c:Coord; pl:int}

let toFuelCell (sn:int) = fun i j ->
    let c = {Coord.x=i+1; y=j+1}
    {FuelCell.c=c; pl=(powerLevel c sn)}



type Grid (N:int, M:int, sn:int) = 
    let internalArray = Array2D.init N M (toFuelCell sn) 
    let cache = ref Map.empty

    let memoize f =        
        fun x ->
            match (!cache).TryFind(x) with
            | Some res -> res
            | None ->
                let res = f x
                cache := (!cache).Add(x,res)
                res

    let rec loop (g:Grid) (s:int, c:Coord) =
        match s with
        | 0 -> 0
        | n when n < 9 -> g.BlockLevel n c
        | _ -> (s,c) |>
                memoize (fun (s':int, c':Coord) ->
                    let s'' = s' / 2    
                    let offset = g.GetOffsets s'' c'               
                    let edges = g.GetRBEdges s' c'
                    offset |> Seq.map (fun e -> loop g (s'',e)) 
                    |> Seq.append (edges |> Seq.map (fun e -> g.BlockLevel 1 e )) 
                    |> Seq.sum ) 

    member __.Item
        with get (c:Coord) = internalArray.[c.x - 1,c.y - 1]
        and  set (c:Coord) (value: FuelCell) = internalArray.[c.x - 1,c.y - 1] <- value

    member this.LoopBlock (size:int) (c:Coord) = 
        seq {
            for y in [c.y..c.y+size-1] do
                for x in [c.x..c.x+size-1] do
                    yield this.[{x=x;y=y}]
        }
    
    member this.BlockLevel (size:int) (c:Coord) =
        this.LoopBlock size c |> Seq.sumBy (fun b -> b.pl)

    member this.MemBlockLevel (s:int) (c:Coord) =         
       loop this (s,c)   

    member this.Blocks (size:int) = 
        seq {
            for y in [0..(Array2D.length2 internalArray) - size] do
                for x in [0..(Array2D.length1 internalArray) - size] do
                    let fc = internalArray.[x,y]
                    yield (fc.c, this.MemBlockLevel size fc.c)
        }

    member __.RowToLine (y:int) =         
        seq {
            for x in [0..(Array2D.length1 internalArray) - 1] do
            let cur = internalArray.[x,y]
            yield sprintf "%i" cur.pl
        } |> String.concat " "

    member this.ToTextLines =        
        seq {
            for y in [0..(Array2D.length2 internalArray) - 1] do
                yield this.RowToLine y
        }

    member __.GetRBEdges (s:int) (c:Coord) =          
        seq {
            if s % 2 <> 0 then
                for y in [c.y..c.y+s-1] do
                   yield {x=c.x+s-1;y=y}
                for x in [c.x..c.x+s-2] do
                    yield {x=x;y=c.y+s-1}
        }

    member __.GetOffsets (s:int) (c:Coord) =
        seq {
            yield c
            yield c.Offset (s,0)
            yield c.Offset (0, s)
            yield c.Offset (s, s)
        }     

let sizeToMax (g:Grid) (s:int) =
    g.Blocks s |> Seq.maxBy (fun (_,pl) -> pl)

let execute = fun d ->
    let g = Grid (300,300,d)
    let (c,_) = 3 |> sizeToMax g
    printfn "Day 11 - part 1: Largest power coordingate is %i,%i" (c.x) (c.y)

    let (s,(c,_)) = [1..20] |> List.map (fun s -> (s, s |> sizeToMax g)) |> List.maxBy (fun (_,(_,p)) -> p)
    printfn "Day 11 - part 2: Largest power coordingate is %i,%i,%i" (c.x) (c.y) s



