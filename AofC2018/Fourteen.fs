﻿module Fourteen

open System.Collections.Generic

type ElfState = {aIndex:int; bIndex:int; aValue:int; bValue:int; count:int; pe:int list}

type Queues = {aQ:Queue<int>; bQ:Queue<int>}

let private toDigitList (n:int) =
    let rec loop (acc:int list) (s:int) = 
        let s' = s / 10
        match s' with
        | 0 -> (s::acc) 
        | _ -> loop ((s % 10)::acc) s'
    loop [] n

let private toBigInt (ls:int list) =
    ls |> List.rev |> List.mapi (fun i d -> bigint.Pow(bigint 10,i) * bigint d) |> List.sum

let private sumToDigitList (a:int, b:int) =
    (a + b) |> toDigitList

let private buildPE (state:ElfState) (sumList:int list) = 
    (sumList |> List.rev) @ state.pe |> List.take state.pe.Length

let private buildQueues (state:ElfState,ls:int list) =    
    let tA = ls |> List.skip (state.aIndex + 1)
    let tB = ls |> List.skip (state.bIndex + 1)
    let aQ = Queue<int>(tA)
    let bQ = Queue<int>(tB)
    ({Queues.aQ=aQ;bQ=bQ}, ls |> List.rev, state)

let private enqueue (queues:Queues) (ls:seq<int>) =
    ls |> Seq.iter (fun n -> 
        queues.aQ.Enqueue n
        queues.bQ.Enqueue n)

let private enqueueOne (queue:Queue<int>) (s:seq<int>) =
    s |> Seq.iter (fun n -> queue.Enqueue n)

let private dequeue2 (count:int) (queue:Queue<int>) (rls:int list) = 
    let rec loop (r:int) (q:Queue<int>) = 
        if q.Count = 0 then
            enqueueOne q (rls |> Seq.rev)
        let cur = q.Dequeue()
        let r' = r - 1
        match r' with 
        | 0 -> cur
        | _ -> loop r' q
    loop count queue

let private addToHead (ls:int list) (add:int list) = 
    let rec loop (acc:int list) (add':int list) =
        match add' with
        | [] -> acc
        | h::t -> loop (h::acc) t
    loop ls add

let private runQueueRound (queues:Queues, rls:int list, state:ElfState) =
    let sumList = (state.aValue, state.bValue) |> sumToDigitList
    enqueue queues sumList
    let rls' = addToHead rls sumList
    let count' = state.count + sumList.Length
    let aStep = state.aValue + 1
    let aValue' = dequeue2 aStep queues.aQ rls'
    let aIndex' = state.aIndex + aStep
    let bStep = state.bValue + 1
    let bValue' = dequeue2 bStep queues.bQ rls'
    let bIndex' = state.bIndex + bStep
    let pe' = buildPE state sumList
    let state' = {state with 
                    aIndex = aIndex';
                    bIndex = bIndex';
                    aValue = aValue';
                    bValue = bValue';
                    count = count';
                    pe=pe'}
    (queues, rls', state')

let private runQueueToCount (max:int) (state:ElfState,ls:int list) = 
    let rec loop (t:int) (queues:Queues, rls:int list, state':ElfState) =
        match state'.count = t with
        | true -> (state', rls |> List.rev)
        | false ->            
                let r = runQueueRound (queues, rls, state')
                loop t r
    let init = buildQueues (state,ls)
    loop max init

let rec countRecipes (max:int) (state:ElfState,ls:int list) =        
    runQueueToCount max (state,ls)

let rec listStartsWith (segment:int list) (ls:int list) =
    match segment with
    | [] -> true
    | h::t -> 
        match ls with
        | [] -> false
        | lsh::lst -> 
            match h = lsh with
            | true  -> listStartsWith t lst
            | false -> false

let private runQueueToMatch (el:int list) (state:ElfState,ls:int list) = 
    let rec loop (el':int list) (queues:Queues, rls:int list, state':ElfState) =
        match listStartsWith state'.pe el' with
        | true -> (state', rls |> List.rev)
        | false -> 
            let r = runQueueRound (queues, rls, state')
            loop el' r
    let init = buildQueues (state,ls)
    loop el init

let rec matchEnd (el:int list) (state:ElfState,ls:int list) =        
    match listStartsWith state.pe el with
    | true -> (state,ls)
    | false ->
        let r = runQueueToMatch el (state,ls)
        matchEnd el r

let getScoresAfter (count:int) (numAfter:int) (state:ElfState,ls:int list) =        
    let (_,ls') = countRecipes (count+numAfter) (state,ls)
    ls' |> List.skip count |> toBigInt

let getCountBeforeMatch (toMatch:int) (state:ElfState,ls:int list) =        
    let endL = toMatch |> toDigitList |> List.rev
    let initPE = List.init endL.Length (fun _ -> -1)
    let state' = {state with pe=initPE}
    let (state'',_) = matchEnd endL (state',ls)
    state''.count - endL.Length

let execute = fun d ->
    let list = [3;7]
    let state = {aIndex=0;bIndex=1;aValue=3;bValue=7;count=2;pe=[-1]}
    let r = getScoresAfter d 10 (state,list)
    printfn "%A" r

   
    let r2 = getCountBeforeMatch d (state,list)
    printfn "%A" r2

let input = 652601

