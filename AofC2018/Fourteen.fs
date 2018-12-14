module Fourteen

open System.Collections.Generic

type ElfState = {aIndex:int; bIndex:int; aValue:int; bValue:int; count:int}

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

let private forward (i:int) (count:int) (length:int) (ls:int list) = 
    let i' = (i + count) % length
    (i', ls.Item i')

let private runRound (state:ElfState,ls:int list) =
    let sumList = (state.aValue, state.bValue) |> sumToDigitList
    let ls' = ls @ sumList
    let count' = state.count + sumList.Length
    let (aIndex',aValue') = ls' |> forward state.aIndex (state.aValue + 1) count'
    let (bIndex',bValue') = ls' |> forward state.bIndex (state.bValue + 1) count'    
    ({state with
        aIndex = aIndex'; 
        bIndex=bIndex'; 
        aValue=aValue'; 
        bValue=bValue';
        count=count'
     }, ls')

let private buildQueues (state:ElfState,ls:int list) =    
    let tA = ls |> List.skip (state.aIndex + 1)
    let tB = ls |> List.skip (state.bIndex + 1)
    let aQ = Queue<int>(tA)
    let bQ = Queue<int>(tB)
    ({Queues.aQ=aQ;bQ=bQ}, ls |> List.rev, state)

let private enqueue (queues:Queues) (ls:int list) =
    ls |> List.iter (fun n -> 
        queues.aQ.Enqueue n
        queues.bQ.Enqueue n)

let private dequeue (count:int) (queue:Queue<int>) = 
    let rec loop (r:int) (q:Queue<int>) = 
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
    let count' = state.count + sumList.Length
    let aStep = state.aValue + 1
    let aValue' = dequeue aStep queues.aQ
    let aIndex' = state.aIndex + aStep
    let bStep = state.bValue + 1
    let bValue' = dequeue bStep queues.bQ
    let bIndex' = state.bIndex + bStep
    let state' = {state with 
                    aIndex = aIndex';
                    bIndex = bIndex';
                    aValue = aValue';
                    bValue = bValue';
                    count = count'}
    (queues, addToHead rls sumList, state')

let private runQueueToCount (max:int) (state:ElfState,ls:int list) = 
    let rec loop (t:int) (queues:Queues, rls:int list, state':ElfState) =
        match state'.count = t with
        | true -> (state', rls |> List.rev)
        | false -> 
            if queues.aQ.Count < 18 || queues.bQ.Count < 18 then
                (state', rls |> List.rev)
            else
                let r = runQueueRound (queues, rls, state')
                loop t r
    let init = buildQueues (state,ls)
    loop max init

let rec countRecipes (max:int) (state:ElfState,ls:int list) =        
    match state.count = max with
    | true -> (state,ls)
    | false ->
        let aDelta = state.count - state.aIndex - 1
        let bDelta = state.count - state.bIndex - 1
        if aDelta > 36 && bDelta > 36 then
            let r = runQueueToCount max (state,ls)
            countRecipes max r
        else
            countRecipes max (runRound (state,ls))

let rec getScoresAfter (count:int) (numAfter:int) (state:ElfState,ls:int list) =        
    let (_,ls') = countRecipes (count+numAfter) (state,ls)
    ls' |> List.skip count |> toBigInt

let execute = fun d ->
    let list = [3;7]
    let state = {aIndex=0;bIndex=1;aValue=3;bValue=7;count=2}
    let r = getScoresAfter d 10 (state,list)
    printfn "%A" r

let input = 652601

