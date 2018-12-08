open System

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.


[<EntryPoint>]
let main argv = 

    let inputs = 
        Seq.initInfinite (fun _ -> Console.ReadLine()) 
        |> Seq.takeWhile ((<>) null) 
        |> Seq.takeWhile ((<>) "exit")

    let selectDay = fun d ->
        match d with
           | "1" -> One.dataSet |> One.execute
           | "2" -> Two.dataSet |> Two.execute
           | "3" -> Three.dataSet |> Three.execute
           | "4" -> Four.dataSet |> Four.execute
           | "5" -> Five.dataSet |> Five.execute
           | "6" -> Six.dataSet |> Six.execute
           | "7" -> Seven.dataSet |> Seven.execute
           | _ -> Console.WriteLine "Unknown"

    Console.WriteLine "Enter day number:" 
    inputs |> Seq.iter selectDay
    0 // return an integer exit code


