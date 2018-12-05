open System

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv = 
    let data = One.dataSet
    let values = One.splitLines data |> One.textLinesSeqToInt
    printfn "Day 1 - Part 1: %i (should be 536)" (One.parseAndCompute values)
    printfn "Day 1 - Part 2: First repeat frequency is %i" (One.firstRepeatFreq values)

    let data = Two.dataSet
    let ids = One.splitLines data
    printfn "Day 2 - Part 1: Sum Check %i" (Two.countAccumulator ids)
    printfn "Day 2 - Part 2: Common %A" (Two.commonChars (List.ofSeq ids))

    let data = Three.dataSet
    let lines = One.splitLines data
    let grid = Three.Grid (2000,2000)
    
    Three.parseTextSeq lines |>  Three.populateGrid grid
    printfn "Day 3 - Part 1: Overlapped scrap squares %A" (grid.Count (fun i -> i > 1))

    Console.ReadLine() |> ignore
    0 // return an integer exit code
