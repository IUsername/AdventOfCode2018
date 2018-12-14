module Fourteen

type ElfState = {aIndex:int; bIndex:int; aValue:int; bValue:int; count:int}

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

let rec countRecipes (max:int) (state:ElfState,ls:int list) =        
    match state.count = max with
    | true -> (state,ls)
    | false -> countRecipes max (runRound (state,ls))

let rec getScoresAfter (count:int) (numAfter:int) (state:ElfState,ls:int list) =        
    let (_,ls') = countRecipes (count+numAfter) (state,ls)
    ls' |> List.skip count |> toBigInt

let execute = fun d ->
    let list = [3;7]
    let state = {aIndex=0;bIndex=1;aValue=3;bValue=7;count=2}
    let r = getScoresAfter d 10 (state,list)
    printfn "%A" r

let input = 652601

