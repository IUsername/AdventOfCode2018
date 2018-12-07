module Seven

type Dependency = {depId:string; stepId:string}

type Task = {depId:string; remaining:int}

type Worker =
    | Idle
    | Working of Task

let addNode (id:string) (g:Map<string,Set<string>>) =
    match Map.tryFind id g with 
    | None -> Map.add id Set.empty g
    | Some _ -> g

let addDep (dep:Dependency) (g:Map<string,Set<string>>) =
    let (depId, stepId) = (dep.depId, dep.stepId)
    let g' = 
        match Map.tryFind depId g with 
        | None -> addNode depId g
        | Some _ -> g
    match Map.tryFind stepId g' with
    | None -> Map.add stepId (Set.singleton depId) g'
    | Some deps -> Map.add stepId (Set.add depId deps) g'

let buildGraph s =
    let rec buildGraph' (deps:Dependency list) (g:Map<string,Set<string>>) =        
        match deps with
        | h::t -> addDep h g |> buildGraph' t
        | [] -> g
    buildGraph' s Map.empty  

let nodes (g:Map<string,Set<string>>) = 
    Map.fold (fun xs k _ -> k::xs) [] g 

let noDeps (g:Map<string,Set<string>>) =
    let nodes = nodes g
    let nodesSet = Set.ofList nodes
    nodes |> List.filter (fun depId -> Map.find depId g |> Set.intersect nodesSet |> Set.isEmpty)

let stepTime (ident:string) =
    let charVal = ident.Chars 0 |> int
    60 + charVal - (int 'A') + 1

let topoSort (g:Map<string,Set<string>>) =
    let rec topoSort' (g':Map<string,Set<string>>, order:string list, rts:string list) =
        if List.isEmpty rts then
            order |> List.rev
        else 
            let n = List.min rts
            let order' = n::order
            let g'' = Map.remove n g'
            let rts' = noDeps g''
            topoSort' (g'', order', rts')
    topoSort' (g, [], noDeps g) |> List.fold (+) ""

let rec performWork (g:Map<string,Set<string>>, order:string list, rts:string list, worker:Worker, inProg:Set<string>) =
    match worker with
    | Working t ->
        match t.remaining with
        | 1 -> 
            let n = t.depId
            let order' = n::order
            let g' = Map.remove n g
            let rts' = noDeps g'
            let inProg' = Set.remove n inProg
            (g', order', rts', Idle, inProg')
        | r -> 
            let t' = {t with remaining=r-1}
            (g, order, rts, Working t', inProg)
    | _ -> (g, order, rts, worker, inProg)

let rec takeWork (g:Map<string,Set<string>>, order:string list, rts:string list, worker:Worker, inProg:Set<string>) =
    let avail = rts |> List.filter (inProg.Contains >> not)
    match worker with
    | Idle when not (avail.IsEmpty) -> 
        let n = avail |> List.min
        let inProg' = Set.add n inProg    
        let worker' = Working {depId=n; remaining=(stepTime n) }
        (g, order, rts, worker', inProg')   
    | _ -> (g, order, rts, worker, inProg)

let performAllWork (g:Map<string,Set<string>>, order:string list, rts:string list, workers:Worker list, inProg:Set<string>) =
    let rec workLoop (acc:Worker list) (g':Map<string,Set<string>>, order':string list, rts':string list, workers':Worker list, inProg':Set<string>) =
        match workers' with
        | h::t -> 
            let (g'',order'',rts'',w', inProg'') = performWork (g',order',rts',h, inProg')
            workLoop (w'::acc) (g'', order'', rts'', t, inProg'') 
        | [] ->
            (g',order',rts',acc |> List.rev, inProg')
    workLoop [] (g, order, rts, workers, inProg)

let takeAllWork (g:Map<string,Set<string>>, order:string list, rts:string list, workers:Worker list, inProg:Set<string>) =
    let rec takeLoop (acc:Worker list) (g':Map<string,Set<string>>, order':string list, rts':string list, workers':Worker list, inProg':Set<string>) =
        match workers' with
        | h::t -> 
            let (g'',order'',rts'',w', inProg'') = takeWork (g',order',rts',h, inProg')
            takeLoop (w'::acc) (g'', order'', rts'', t, inProg'') 
        | [] ->
            (g',order',rts',acc |> List.rev, inProg')
    takeLoop [] (g, order, rts, workers, inProg)
    
let allIdle = List.forall (function Idle -> true | _ -> false)  

let topoSortWithTime (workers:int) (g:Map<string,Set<string>>)  =
    let rec topoSort' (elapsed:int) state =
        let (_, order', rts', workers', _) = state
        if List.isEmpty rts' && workers' |> allIdle then
            (elapsed, order' |> List.rev |> List.fold (+) "")
        else             
            let state' = state |> takeAllWork  |> performAllWork |> takeAllWork             
            topoSort' (elapsed + 1) state'
    topoSort' 0 (g, [], noDeps g, List.init workers (fun _ -> Idle), Set.empty) 

let parseStep text = 
    match text with
    | Parsing.Regex @".+\s([A-Z]{1})\s.+\s([A-Z]{1})\s.+" [depId; stepId] -> 
        Some { Dependency.depId = depId; stepId = stepId }
    | _ -> None

let dataSet' = @"
Step F must be finished before step E can begin.
Step C must be finished before step A can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin."

//JDEKPFABTUHOQSXVYMLZCNIGRW
let dataSet = @"
Step J must be finished before step E can begin.
Step X must be finished before step G can begin.
Step D must be finished before step A can begin.
Step K must be finished before step M can begin.
Step P must be finished before step Z can begin.
Step F must be finished before step O can begin.
Step B must be finished before step I can begin.
Step U must be finished before step W can begin.
Step A must be finished before step R can begin.
Step E must be finished before step R can begin.
Step H must be finished before step C can begin.
Step O must be finished before step S can begin.
Step Q must be finished before step Y can begin.
Step V must be finished before step W can begin.
Step T must be finished before step N can begin.
Step S must be finished before step I can begin.
Step Y must be finished before step W can begin.
Step Z must be finished before step C can begin.
Step M must be finished before step L can begin.
Step L must be finished before step W can begin.
Step N must be finished before step I can begin.
Step I must be finished before step G can begin.
Step C must be finished before step G can begin.
Step G must be finished before step R can begin.
Step R must be finished before step W can begin.
Step Z must be finished before step R can begin.
Step Z must be finished before step N can begin.
Step G must be finished before step W can begin.
Step L must be finished before step G can begin.
Step Y must be finished before step R can begin.
Step P must be finished before step I can begin.
Step C must be finished before step W can begin.
Step T must be finished before step G can begin.
Step T must be finished before step R can begin.
Step V must be finished before step Z can begin.
Step L must be finished before step C can begin.
Step K must be finished before step I can begin.
Step J must be finished before step I can begin.
Step Q must be finished before step C can begin.
Step F must be finished before step A can begin.
Step H must be finished before step Y can begin.
Step M must be finished before step N can begin.
Step P must be finished before step H can begin.
Step M must be finished before step C can begin.
Step V must be finished before step Y can begin.
Step O must be finished before step V can begin.
Step O must be finished before step Q can begin.
Step A must be finished before step G can begin.
Step T must be finished before step Z can begin.
Step K must be finished before step R can begin.
Step H must be finished before step O can begin.
Step O must be finished before step Y can begin.
Step O must be finished before step C can begin.
Step K must be finished before step P can begin.
Step P must be finished before step F can begin.
Step E must be finished before step M can begin.
Step M must be finished before step I can begin.
Step T must be finished before step W can begin.
Step P must be finished before step L can begin.
Step A must be finished before step O can begin.
Step X must be finished before step V can begin.
Step S must be finished before step G can begin.
Step A must be finished before step Y can begin.
Step J must be finished before step R can begin.
Step K must be finished before step F can begin.
Step J must be finished before step A can begin.
Step P must be finished before step C can begin.
Step E must be finished before step N can begin.
Step F must be finished before step Y can begin.
Step J must be finished before step D can begin.
Step H must be finished before step Z can begin.
Step U must be finished before step H can begin.
Step J must be finished before step T can begin.
Step V must be finished before step G can begin.
Step Z must be finished before step I can begin.
Step H must be finished before step W can begin.
Step B must be finished before step R can begin.
Step F must be finished before step B can begin.
Step X must be finished before step C can begin.
Step L must be finished before step R can begin.
Step F must be finished before step U can begin.
Step D must be finished before step N can begin.
Step P must be finished before step O can begin.
Step B must be finished before step O can begin.
Step F must be finished before step C can begin.
Step H must be finished before step L can begin.
Step O must be finished before step N can begin.
Step J must be finished before step Y can begin.
Step H must be finished before step N can begin.
Step O must be finished before step L can begin.
Step I must be finished before step W can begin.
Step J must be finished before step H can begin.
Step D must be finished before step Z can begin.
Step F must be finished before step W can begin.
Step X must be finished before step W can begin.
Step Y must be finished before step M can begin.
Step T must be finished before step M can begin.
Step U must be finished before step G can begin.
Step L must be finished before step I can begin.
Step N must be finished before step W can begin.
Step E must be finished before step C can begin.
"