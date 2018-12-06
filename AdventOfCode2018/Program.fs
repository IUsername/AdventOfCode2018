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
    let mostSleep = stamped |> Four.minutesAsleepPerShiftSeq |> Four.sumShifts |> Seq.sortByDescending (fun s -> s.minutes) |> Seq.head
    let sleepy = stamped |> Four.findMostSleepyMinuteById mostSleep.id
    printfn "Day 4 - Part 1: Most sleepy guard is #%i at minute %i multiplied to %i" sleepy.id sleepy.minute (sleepy.id * sleepy.minute)
    let consistent = stamped |> Four.sleepyPerGuard |> List.sortByDescending (fun p -> p.freq) |> List.head
    printfn "Day 4 - Part 2: Most consistent is guard #%i at minute %i multiplied to %i" consistent.id consistent.minute (consistent.id * consistent.minute)

    let final = Five.dataSet |> Five.toCharList |> Five.reduce
    printfn "Day 5 - Part 1: Unit count %i" final.Length

    

    Console.ReadLine() |> ignore
    0 // return an integer exit code
