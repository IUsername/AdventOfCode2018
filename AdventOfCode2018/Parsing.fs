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

