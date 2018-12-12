module Twelve

open System
open System.Collections

module Seq =
    let ofBitArray (bitArray:BitArray) = seq { 
        for i=0 to bitArray.Length-1 do yield bitArray.Get(i) 
    }

type Rules = Map<int,bool>
         
let private bitSeqToInt (bs:seq<bool>) =    
    bs
    |> Seq.mapi (fun i b -> if b then 1 <<< i else 0) 
    |> Seq.fold (fun p b -> p ||| b) 0  
    
let private bitSeqToInt64 (bs:seq<bool>) =    
    bs
    |> Seq.mapi (fun i b -> if b then 1L <<< i else 0L) 
    |> Seq.fold (fun p b -> p ||| b) 0L  

let private getBit (max:int) (ba:BitArray) (index:int) =
    match index with
    | _ when index < 0 -> false
    | _ when index > max -> false
    | _ -> ba.[index]   
    
let private surround (width:int) (index:int) (max:int) (ba:BitArray) =
    [(index-width)..(index+width)] |> Seq.map (getBit max ba)

let private step (width:int) (r:Rules) (startIndex:int, ba:BitArray) =
    seq {
        let maxIndex = ba.Length - 1
        for index=0-width to maxIndex+width do
            let iVal = surround width index maxIndex ba |> bitSeqToInt
            yield (index+startIndex,r.Item iVal)
        }
        
let private toGen (p:seq<int*bool>) =
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
    BitArray(str.ToCharArray() |> Array.map charToBit)

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
    let mutable priorHash = init |> Seq.ofBitArray |> bitSeqToInt64
    let mutable continueLooping = true
    let mutable stopForHashCheck = false
    let mutable gen = 1L
    while continueLooping do
        if gen = maxGen then
            continueLooping <- false
        let (s,ba,hash) = step 2 rules result |> toGen        
        if hash = priorHash then
            continueLooping <- false
            stopForHashCheck <- true
        gen <- gen + 1L      
        result <- (s,ba)  
        priorHash <- hash
    
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
    let rules = Rules(lines |> Seq.skip 1 |> Seq.choose parseRule) 

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