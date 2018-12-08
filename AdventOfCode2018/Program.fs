open System

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.


[<EntryPoint>]
let main argv = 

    let inputs = 
        Seq.initInfinite (fun _ -> Console.ReadLine()) 
        |> Seq.takeWhile ((<>) null) 
        |> Seq.takeWhile ((<>) "exit")

    let duration f d = 
        let timer = new Diagnostics.Stopwatch()
        timer.Start()
        let r = f(d)
        (r, timer.Elapsed)

    let selectDay d = 
        match d with
           | "1" -> One.dataSet |> One.execute; true
           | "2" -> Two.dataSet |> Two.execute; true
           | "3" -> Three.dataSet |> Three.execute; true
           | "4" -> Four.dataSet |> Four.execute; true
           | "5" -> Five.dataSet |> Five.execute; true
           | "6" -> Six.dataSet |> Six.execute; true
           | "7" -> Seven.dataSet |> Seven.execute; true
           | _ -> false

    let success = fun (s:bool,t:TimeSpan) -> 
        if s then printfn "Done in %ims" t.Milliseconds else Console.WriteLine "Error"

    Console.WriteLine "Enter day number:" 
    inputs |> Seq.map (duration selectDay) |> Seq.iter success
    0 // return an integer exit code


