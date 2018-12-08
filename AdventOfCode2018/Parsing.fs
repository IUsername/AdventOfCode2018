module Parsing

open System
open System.Text.RegularExpressions

let splitLines (text: string) = 
    text.Split(Environment.NewLine.ToCharArray(), StringSplitOptions.RemoveEmptyEntries)   
    |> Seq.ofArray  

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let (|RegexMany|_|) pattern input =
    let m = Regex.Matches(input, pattern, RegexOptions.Compiled) |> Seq.cast<Match> 
    if Seq.isEmpty m then None else Some (m |> Seq.map (fun m -> m.Value) |> Seq.toList)

