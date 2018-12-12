module Twelve

open System
open System.Collections

module Seq =
    let ofBitArray (bitArray:BitArray) = seq { 
        for i=0 to bitArray.Length-1 do yield bitArray.Get(i) 
    }

type Rules = Map<int,bool>
         
let bitSeqToInt (bs:seq<bool>) =    
    bs
    |> Seq.mapi (fun i b -> if b then 1 <<< i else 0) 
    |> Seq.fold (fun p b -> p ||| b) 0  
    
let bitSeqToInt64 (bs:seq<bool>) =    
    bs
    |> Seq.mapi (fun i b -> if b then 1L <<< i else 0L) 
    |> Seq.fold (fun p b -> p ||| b) 0L  

let getBit (m:int) (ba:BitArray) (i:int) =
    match i with
    | _ when i < 0 -> false
    | _ when i > m -> false
    | _ -> ba.[i]   
    
let surround (w:int) (bI:int) (max:int) (ba:BitArray) =
    seq {
        for s=(-w) to w do
           let i = bI + s
           let b = getBit max ba i
           yield b
        }

let eval (w:int) (r:Rules) (sI:int, ba:BitArray) =
    seq {
        let maxIndex = ba.Length - 1
        for bit=0-w to maxIndex+w do
            let iVal = surround w bit maxIndex ba |> bitSeqToInt
            let p = r.TryFind iVal
            match p with
            | Some v -> yield (bit+sI,v)
            | None -> yield (bit+sI, false)
        }
        
let toGen (p:seq<int*bool>) =
    let trimmed = p |> Seq.skipWhile (snd >> not)
    let startIndex = trimmed |> Seq.head |> fst
    let lastIndex = trimmed |> Seq.findIndexBack (snd)
    let final = trimmed |> Seq.take (lastIndex+1)
    let ba = final |> Seq.map (snd) |> Array.ofSeq |> BitArray 
    let hash = ba |> Seq.ofBitArray |> bitSeqToInt64 
    (startIndex, ba, hash)

let private charToBit (c:Char) =
    match c with
    | '#' -> true
    | _ -> false

let private strToBitArray (str:string) =
    let bits = str.ToCharArray() |> Array.map charToBit
    BitArray(bits)

let private strToBit (str:string) =
    str.Chars 0 |> charToBit

let private bitArrayToInt (ba:BitArray) = 
    ba |> Seq.ofBitArray |> bitSeqToInt

let private potCount (startIndex:int64, ba:BitArray) = 
    let mutable count = 0L
    for bit=0 to ba.Length-1 do 
        if ba.Get(bit) then 
            count <- count + startIndex + int64 bit
    count
    
let private rulesToMap(r:seq<(int*bool)>) = Map<int,bool>(r)

let private parseState line =
    match line with
    | Parsing.Regex @"initial state: ([.#]+)" [s] ->
        Some (s |> strToBitArray)
    | _ -> None

let private parseRule line =
    match line with
    | Parsing.Regex @"([.#]+) => ([.#])" [m;r] ->
        Some ((m |> strToBitArray |> bitArrayToInt), (r |> strToBit))
    | _ -> None

let private runSim (rules:Rules) (init:BitArray) (maxGen:int64) = 
    let mutable result = (0,init)
    let mutable priorHash = 0L
    let mutable continueLooping = true
    let mutable stopForHashCheck = false
    let mutable gen = 1L
    while continueLooping do
        if gen = maxGen then
            continueLooping <- false
        let (s,ba,h) = eval 2 rules result |> toGen        
        if h = priorHash then
            continueLooping <- false
            stopForHashCheck <- true
        gen <- gen + 1L      
        result <- (s,ba)  
        priorHash <- h
    
    let (sI,ba) = result
    
    if stopForHashCheck then
        let rGen = maxGen - gen + 1L
        let r = (int64 sI + rGen, ba)
        (r |> potCount)
    else
        let r = (int64 sI, ba)
        (r |> potCount)

let execute = fun d ->
    let lines = d |> Parsing.splitLines 
    let initLine = lines |> Seq.choose parseState |> Seq.head
    let rules = lines |> Seq.skip 1 |> Seq.choose parseRule |> rulesToMap    

    printfn "Day 12 - part 1: Pot sum %i" (runSim rules initLine 20L)
    printfn "Day 12 - part 2: Pot sum %i" (runSim rules initLine 50000000000L)
    

let testSet = @"
initial state: #..#.#..##......###...###

...## => #
..#.. => #
.#... => #
.#.#. => #
.#.## => #
.##.. => #
.#### => #
#.#.# => #
#.### => #
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #
"

let dataSet = @"
initial state: ##.######...#.##.#...#...##.####..###.#.##.#.##...##..#...##.#..##....##...........#.#.#..###.#

.###. => #
#.##. => .
.#.## => #
...## => .
###.# => #
##.## => .
..... => .
#..#. => #
..#.. => #
#.### => #
##.#. => .
..#.# => #
#.#.# => #
.##.# => #
.#..# => #
#..## => #
##..# => #
#...# => .
...#. => #
##### => .
###.. => #
#.#.. => .
....# => .
.#### => #
..### => .
..##. => #
.##.. => .
#.... => .
####. => #
.#.#. => .
.#... => #
##... => #"