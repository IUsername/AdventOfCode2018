module Fourteen

open System.Collections.Generic

let private toDigitList8 n =
    let rec loop (acc:int8 list) s = 
        let s' = s / 10
        match s' with
        | 0 -> ((int8 s)::acc) 
        | _ -> loop ((int8 (s % 10))::acc) s'
    loop [] n

let private toBigInt (ls:seq<_>) =
    ls |> Seq.rev |> Seq.mapi (fun i d -> bigint.Pow(bigint 10,i) * bigint ((int)d)) |> Seq.sum      

let genSeq =
    seq {
        let init = [3y;7y]        
        let mutable elf1 = 0
        let mutable elf2 = 1
        let mutable scores = List<int8>(init)
        let mutable len = scores.Count
        yield! init
        while true do
            let v1 = scores.[elf1]
            let v2 = scores.[elf2]    
            let tail = toDigitList8 (int(v1+v2))
            scores.AddRange (tail)
            yield! tail
            len <- scores.Count
            elf1 <- (elf1 + int v1 + 1) % len
            elf2 <- (elf2 + int v2 + 1) % len
        }

let execute = fun d ->
    let r1 = genSeq |> Seq.skip d |> Seq.take 10 |> toBigInt
    printfn "Day 14 - part 1: Score %A" r1
    let toMatch = toDigitList8(d) |> Array.ofList
    let r2 = genSeq |> Seq.windowed toMatch.Length |> Seq.findIndex (fun w -> w = toMatch)
    printfn "Day 14 - part 2: Count to left %i" r2

let input = 652601

