open System

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv = 
    let data = One.dataSet
    let values = Parsing.splitLines data |> One.textLinesSeqToInt
    printfn "Day 1 - Part 1: %i (should be 536)" (Seq.sum values)
    printfn "Day 1 - Part 2: First repeat frequency is %i" (One.firstRepeatFreq values)

    let data = Two.dataSet
    let ids = Parsing.splitLines data
    printfn "Day 2 - Part 1: Sum Check %i" (Two.countAccumulator ids)
    printfn "Day 2 - Part 2: Common %A" (Two.commonChars (List.ofSeq ids))

    let data = Three.dataSet
    let lines = Parsing.splitLines data
    let scraps = Three.parseTextSeq lines
    let grid = Three.Grid (2000,2000)    
    Three.populateGrid grid scraps
    printfn "Day 3 - Part 1: Overlapped scrap squares %A" (grid.Count (fun i -> i > 1))
    printfn "Day 3 - Part 2: Non-overlapped scrap ID %i" (Three.notOverlappedId grid scraps).id

    let events = Four.dataSet |> Parsing.splitLines |> Four.parseEvents
    let ordered = events |> Seq.sortBy (fun e -> e.time)
    let stamped = ordered |> Four.stampWithId |> List.ofSeq
    let mostSleep = stamped |> Four.minutesAsleepPerShiftSeq |> Four.sumShifts |> Seq.maxBy (fun s -> s.minutes)
    let sleepy = stamped |> Four.findMostSleepyMinuteById mostSleep.id
    printfn "Day 4 - Part 1: Most sleepy guard is #%i at minute %i multiplied to %i" sleepy.id sleepy.minute (sleepy.id * sleepy.minute)
    let consistent = stamped |> Four.sleepyPerGuard |> List.maxBy (fun p -> p.freq)
    printfn "Day 4 - Part 2: Most consistent is guard #%i at minute %i multiplied to %i" consistent.id consistent.minute (consistent.id * consistent.minute)

    let dataList = Five.dataSet |> Five.toCharList
    let reduced = dataList |> Five.reduce
    printfn "Day 5 - Part 1: Unit count %i" reduced.Length
    let (char, len) = reduced |> Five.maxPolymnerRemovalImpact 
    printfn "Day 5 - Part 2: Greatest impact was %A with final size of %i" char len    

    let pegs = Six.dataSet |> Parsing.splitLines |> Seq.map Six.parseCoord |> Six.coordToPegSeq
    let map = Six.Locations (400,400)
    pegs |> Seq.iter (fun p -> map.AddPeg p)
    let edgeIds = map.EdgeIds |> Set.ofSeq
    let areas = map.Areas |> Seq.filter (fun (id,_) -> not (edgeIds.Contains id))
    let largest = areas |> Seq.map snd |> Seq.max
    printfn "Day 6 - Part 1: Largest non-infinite size is %i" largest

    let nearArea = map.Distances 10000 |> Seq.length
    printfn "Day 6 - Part 2: Largest centralized size is %i" nearArea

    Console.ReadLine() |> ignore
    0 // return an integer exit code
