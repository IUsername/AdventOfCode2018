module Seven

type StepInfo = {id:string; before:string}

let addNode (id:string) (g:Map<string,Set<string>>) =
    match Map.tryFind id g with 
    | None -> Map.add id Set.empty g
    | Some _ -> g

let buildGraph s =
    let rec buildGraph' (g:Map<string,Set<string>>) (steps:StepInfo list) =
        match steps with
        | h::t when (g.ContainsKey h.id) ->
                let deps' = (g.Item h.id).Add h.before
                let map' = g.Add (h.id, deps')
                buildGraph' map' t
        | h::t ->   let map' = g.Add (h.id, Set.singleton h.before) |> addNode h.before
                    buildGraph' map' t
        | [] -> g
    buildGraph' Map.empty s   

let nodes (g:Map<string,Set<string>>) = 
    Map.fold (fun xs k _ -> k::xs) [] g 

let roots  (g:Map<string,Set<string>>) = 
    List.filter (fun n -> not (Map.exists (fun _ v -> Set.contains n v) g)) (nodes g) |> List.sort

let topoSort (g:Map<string,Set<string>>) =
    let rec dfs (g':Map<string,Set<string>>, order:string list, rts:string list) =
        if List.isEmpty rts then
            order
        else 
            let n = List.head rts
            let order' = n::order
            let g'' = Map.remove n g'
            let rts' = roots g''
            dfs (g'', order', rts')
    dfs (g, [], roots g) |> List.rev |> List.fold (+) ""

let parseStep text = 
    match text with
    | Parsing.Regex @".+\s([A-Z]{1})\s.+\s([A-Z]{1})\s.+" [ident; before] -> 
        Some { StepInfo.id = ident; before = before }
    | _ -> None

let dataSet' = @"
Step F must be finished before step E can begin.
Step C must be finished before step A can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin."

//JDKPFABEUHOQTXVYMLZCNSIGRW
//JDKPFABEUHOQTXVYMLZCNSIGRW
//JDKPFABEUHOQTXVYMLZCNSIGRW
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