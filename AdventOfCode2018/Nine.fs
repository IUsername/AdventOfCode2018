module Nine

[<StructuredFormatDisplay("{Left} {Current} {Right}")>]
type CircleList<'a> (lr:'a list, r:'a list, c:'a) =   
    static member Init(n:'a) = CircleList ([],[], n)
    member __.Left = List.rev lr
    member __.Current = c
    member __.Right = r   
    member __.InsertClockwise(n:'a) = CircleList(c::lr, r, n)
    member __.InsertCounterclockwise(n:'a) = CircleList(lr, c::r, n)
    member __.ToList = (List.rev lr ) @ [c] @ r

    member __.MoveClockwise = 
        match r with
        | h::t -> CircleList(c::lr, t, h)
        | [] -> 
            let l = List.rev lr
            match l with
            | h::t -> CircleList([], t @ [c], h)
            | [] -> CircleList([],[], c)    

    member __.MoveCounterclockwise =
        match lr with
        | h::t -> CircleList(t, c::r, h)
        | [] -> 
            let rr = List.rev r
            match rr with
            | h::t -> CircleList(t @ [c],[],h)
            | [] -> CircleList([],[],c)

    member __.RemoveCurrent = 
        match r with
        | h::t -> (c, CircleList(lr, t, h))
        | [] -> 
            let l = List.rev lr
            match l with
            | h::t -> (c, CircleList([], t, h))
            | [] -> failwith "Cannot remove current"      
    
let placeMarble (m:int) (c:CircleList<int>) = 
    match m % 23 with
    | 0 -> 
        let mutable cm = c
        for _ in 1..7 do
            cm <- cm.MoveCounterclockwise
        let (r,cm) = cm.RemoveCurrent
        (m+r,cm)
    | _ -> 
        let c' = c.MoveClockwise
        (0, c'.InsertClockwise m)

let findWinner (players, maxMarble) = 
    let mutable l = CircleList.Init 0
    let players = Array.create players 0L
        
    for i in 1..maxMarble do
        let pI = (i-1) % players.Length
        let (p,l') = placeMarble i l
        players.[pI] <- players.[pI] + int64 p
        l <- l'  

    Array.max players

let execute = fun d -> 
    findWinner d |> (fun h -> printfn "Day 9 - part 1: High score of %i for %i player with last marble of %i" h (fst d) (snd d))
    let (p, m) = (fst d, snd d * 100)
    findWinner (p,m) |> (fun h -> printfn "Day 9 - part 1: High score of %i for %i player with last marble of %i" h p m)
        
let test = 
    findWinner (13, 7999)

let dataSet = (428,70825)

