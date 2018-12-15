module Fourteen

open System.Collections.Generic

let private toDigitList8 (n:int) =
    let rec loop (acc:int8 list) (s:int) = 
        let s' = s / 10
        match s' with
        | 0 -> ((int8 s)::acc) 
        | _ -> loop ((int8 (s % 10))::acc) s'
    loop [] n

let private toBigInt8 (ls:seq<int8>) =
    ls |> Seq.rev |> Seq.mapi (fun i d -> bigint.Pow(bigint 10,i) * bigint ((int)d)) |> Seq.sum
       
let areEqual (a:seq<'t>) (b:seq<'t>) =
    Seq.zip a b |> Seq.forall (fun (i,j) -> i = j)

let gen (d:int) = 
    let mutable scores = List<int8>([3y;7y])
    let mutable elf1 = 0
    let mutable elf2 = 1
    let mutable len = scores.Count
    let mutable working = true
    let mutable shift = 0
    let toMatch = List<int8>(toDigitList8(d))
    let matchCount = toMatch.Count
    while working do
        let v1 = scores.[elf1]
        let v2 = scores.[elf2]      
        scores.AddRange (toDigitList8 (int(v1+v2)))
        len <- scores.Count
        elf1 <- (elf1 + int v1 + 1) % len
        elf2 <- (elf2 + int v2 + 1) % len
        let index = len-matchCount
        if index > 1 then
            // Check end twice since two recipes may have been added from the score
            let endRange = scores.GetRange(index-1, matchCount)
            working <- not (areEqual toMatch endRange )
            if working then
                let endRange = scores.GetRange(index, matchCount)
                working <- not (areEqual toMatch endRange )    
            else
                shift <- -1
    let part1 = scores.GetRange(d, 10) |> toBigInt8
    let part2 = scores.Count - matchCount + shift
    (part1,part2)

let execute = fun d ->
    let (p1,p2) = (gen d)
    printfn "Day 14 - part 1: Score %A" p1
    printfn "Day 14 - part 2: Count to left %i" p2

let input = 652601

