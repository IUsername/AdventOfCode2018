// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =
    let inputs = 
        Seq.initInfinite (fun _ -> Console.ReadLine()) 
        |> Seq.takeWhile ((<>) null) 
        |> Seq.takeWhile ((<>) "")

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
           | "8" -> Eight.dataSet |> Eight.execute; true
           | "9" -> Nine.dataSet |> Nine.execute; true
           | "10" -> Ten.dataSet |> Ten.execute; true
           | "11" -> Eleven.serialNumber |> Eleven.execute; true
           | "12" -> Twelve.dataSet |> Twelve.execute; true
           | "13" -> Thirteen.dataSet |> Thirteen.execute; true
           | "14" -> Fourteen.input |> Fourteen.execute; true
           | "15" -> Fifteen.dataSet |> Fifteen.execute; true
           | "16" -> Sixteen.dataSet |> Sixteen.execute; true
           | _ -> printfn "Value %s not matched" d; false

    let success = fun (s:bool,t:TimeSpan) -> 
        if s then printfn "Done in %ims" (int t.TotalMilliseconds) else Console.WriteLine "Error"

    if argv.Length > 0 then
        printfn "Starting with %s..." argv.[0]
        Seq.ofArray argv |> Seq.map (duration selectDay) |> Seq.iter success
    else
        Console.WriteLine "Enter day number:" 
        inputs |> Seq.map (duration selectDay) |> Seq.iter success
    0 // return an integer exit code
