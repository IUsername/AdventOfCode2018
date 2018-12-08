module Six

type Coord = {x:int; y:int} with    
    member this.DistanceTo (other:Coord) = 
        abs (this.x - other.x) + abs (this.y - other.y)

type Peg = {id:int; coord:Coord}

type DistInfo = {id:int; dist:int}

type Nearby = 
    | Unknown
    | Single of DistInfo
    | Multiple of int

type Locations (N: int, M:int) = 
    let internalArray = Array2D.create N M Unknown
    let distArray = Array2D.create N M 0

    member __.Item
        with get (c:Coord) = internalArray.[c.x,c.y]
        and  set (c:Coord) (value: Nearby) = internalArray.[c.x,c.y] <- value

    member __.AddDist (c:Coord) (d:DistInfo) =  
        let cur = distArray.[c.x,c.y]
        distArray.[c.x,c.y] <- cur + d.dist

    member this.Update (c:Coord) (d:DistInfo) =        
        this.AddDist c d
        match this.[c] with
        | Unknown -> this.[c] <- (Single d)
        | Single p when p.dist > d.dist -> this.[c] <- (Single d)
        | Single p when p.dist = d.dist && p.id <> d.id -> this.[c] <- (Multiple p.dist)
        | Multiple p when p > d.dist -> this.[c] <- (Single d)
        | _ -> ()    

    member this.AddPeg (p:Peg) =
        for x in 0..N-1 do
            for y in 0..M-1 do
                let cur = {x=x; y=y}
                let d = {DistInfo.id=p.id; dist = (cur.DistanceTo p.coord)}
                this.Update cur d    
                
    member __.EdgeCoords =
        seq {
            let xMax = N-1
            let yMax = M-1
            for x in 0..xMax do
                for y in [|0;yMax|] do
                    yield {x=x;y=y}
            for y in 1..yMax-1 do 
                for x in [|0;xMax|] do
                    yield {x=x;y=y}
        }

    member this.EdgeIds = 
        this.EdgeCoords 
        |> Seq.map (fun c -> this.[c]) 
        |> Seq.choose (function | Single dt -> Some dt.id | _ -> None)   
        
    member __.Areas =
        internalArray 
        |> Seq.cast<Nearby> 
        |> Seq.choose (function | Single dt -> Some dt.id | _ -> None)
        |> Seq.countBy id

    member __.Distances max = 
        distArray
        |> Seq.cast<int>
        |> Seq.filter (fun d -> d < max)

let parseCoord text = 
    match text with
    | Parsing.RegexMany @"\d+" [x;y] -> 
        Some {Coord.x = int x; y = int y}
    | _ -> None

let coordToPegSeq c =
    c |> Seq.choose id |> Seq.mapi (fun i el -> {id=i; coord=el})

let execute = fun d ->
    let pegs = d |> Parsing.splitLines |> Seq.map parseCoord |> coordToPegSeq
    let maxX = pegs |> Seq.map (fun p -> p.coord.x) |> Seq.max 
    let maxY = pegs |> Seq.map (fun p -> p.coord.y) |> Seq.max
    let map = Locations (maxX,maxY)
    pegs |> Seq.iter (fun p -> map.AddPeg p)
    let edgeIds = map.EdgeIds |> Set.ofSeq
    let areas = map.Areas |> Seq.filter (fun (id,_) -> not (edgeIds.Contains id))
    let largest = areas |> Seq.map snd |> Seq.max
    printfn "Day 6 - Part 1: Largest non-infinite size is %i" largest
    let nearArea = map.Distances 10000 |> Seq.length
    printfn "Day 6 - Part 2: Largest centralized size is %i" nearArea

let dataSet = @"
177, 51
350, 132
276, 139
249, 189
225, 137
337, 354
270, 147
182, 329
118, 254
174, 280
42, 349
96, 341
236, 46
84, 253
292, 143
253, 92
224, 137
209, 325
243, 195
208, 337
197, 42
208, 87
45, 96
64, 295
266, 248
248, 298
194, 261
157, 74
52, 248
243, 201
242, 178
140, 319
69, 270
314, 302
209, 212
237, 217
86, 294
295, 144
248, 206
157, 118
155, 146
331, 40
247, 302
250, 95
193, 214
345, 89
183, 206
121, 169
79, 230
88, 155"

