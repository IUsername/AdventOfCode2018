module Sixteen

type Registers = {a:int;b:int;c:int;d:int} 
    with static member Default = {a=0;b=0;c=0;d=0}

type Values = {a:int;b:int;c:int}

type Instruction = {opNo:int;a:int;b:int;c:int}

type OpFun = Values -> Registers -> Registers

let private reg (r:Registers) (i:int) =
    match i with | 0 -> r.a | 1 -> r.b | 2 -> r.c | 3 -> r.d | _ -> failwith "invalid register" 

let private setReg (r:Registers) (i:int) (value:int) =
    match i with 
    | 0 -> {r with a = value}
    | 1 -> {r with b = value}
    | 2 -> {r with c = value} 
    | 3 -> {r with d = value} 
    | _ -> failwith "invalid register" 

let addr (v:Values) (r:Registers) = setReg r v.c (reg r v.a + reg r v.b)
let addi (v:Values) (r:Registers) = setReg r v.c (reg r v.a + v.b)
let mulr (v:Values) (r:Registers) = setReg r v.c (reg r v.a * reg r v.b)
let muli (v:Values) (r:Registers) = setReg r v.c (reg r v.a * v.b)
let banr (v:Values) (r:Registers) = setReg r v.c (reg r v.a &&& reg r v.b)
let bani (v:Values) (r:Registers) = setReg r v.c (reg r v.a &&& v.b)
let borr (v:Values) (r:Registers) = setReg r v.c (reg r v.a ||| reg r v.b)
let bori (v:Values) (r:Registers) = setReg r v.c (reg r v.a ||| v.b)
let setr (v:Values) (r:Registers) = setReg r v.c (reg r v.a)
let seti (v:Values) (r:Registers) = setReg r v.c (v.a)
let gtir (v:Values) (r:Registers) = setReg r v.c (if v.a > reg r v.b then 1 else 0)
let gtri (v:Values) (r:Registers) = setReg r v.c (if reg r v.a > v.b then 1 else 0)
let gtrr (v:Values) (r:Registers) = setReg r v.c (if reg r v.a > reg r v.b then 1 else 0)
let eqir (v:Values) (r:Registers) = setReg r v.c (if v.a = reg r v.b then 1 else 0)
let eqri (v:Values) (r:Registers) = setReg r v.c (if reg r v.a = v.b then 1 else 0)
let eqrr (v:Values) (r:Registers) = setReg r v.c (if reg r v.a = reg r v.b then 1 else 0)

let private OpFunctions = [|addr;addi;mulr;muli;banr;bani;borr;bori;setr;seti;gtir;gtri;gtrr;eqir;eqri;eqrr|]

let private executeOp (op:OpFun) (v:Values) (r:Registers) = op v r

let private testOp (op:OpFun) (v:Values) (r:Registers) (target:Registers) = target = (executeOp op v r)

let private passing (ops:OpFun array) (v:Values) (r:Registers) (target:Registers) =
    ops |> Array.map (fun op -> testOp op v r target)

let private flattenPasses (run:(int*bool[]) list) =
    let (opNo,s) = run |> List.head
    let rec loop acc (r:(int*bool[]) list) =
        match r with
        | []   -> (opNo,acc)
        | h::t -> loop (snd h |> Array.zip acc |> Array.map(fun (a,b) -> a&&b)) t
    loop (Array.create s.Length true) run  
    
let private countOfTrue (ar:bool[]) = ar |> Array.sumBy (fun b -> if b then 1 else 0) 

let private determineOps (ops:OpFun[]) (ls:(int*bool[]) list) =
    let rec loop (d:Map<int,OpFun>) (ls':(int*bool[]) list) =   
        match ls' with
        | [] -> d
        | _ ->
            let one = ls' |> List.tryFind (fun (_,ar) -> countOfTrue ar = 1)
            match one with
            | Some (opNo,t) -> 
                let index = t |> Array.findIndex id
                let op = ops.[index]
                let d' = d.Add(opNo,op)
                let ls'' = ls' 
                            |> List.filter (fun (i,_) -> i<>opNo) 
                            |> List.map (fun (i,ar) -> (i, (Array.set ar index false; ar)))
                loop d' ls''
            | None -> failwith "cannot determine all ops"
    loop Map.empty ls
    
let private toValues (i:Instruction) = {Values.a=i.a;b=i.b;c=i.c}

let private runProgram (d:Map<int,OpFun>) (ls:Instruction list) = 
    let init = Registers.Default
    let rec loop (r:Registers) (ls':Instruction list) =
        match ls' with
        | [] -> r
        | h::t -> 
            let op = d.[h.opNo]
            let v = toValues h
            loop (executeOp op v r) t
    loop init ls

let private splitDataSet (split:string) (text:string) =  
    let mutable remaining = text
    let mutable parts = []
    while remaining.Length > 0 do
          let splitIndex = remaining.IndexOf split
          if splitIndex > -1 then
            parts <- (remaining.Substring (0,splitIndex)) :: parts
            remaining <- remaining.Substring (
                                splitIndex+split.Length, 
                                remaining.Length - splitIndex-split.Length)
          else
            parts <- remaining :: parts
            remaining <- ""
    parts |> List.rev

let private parseToRegisters (line:string) =
    match line with
    | Parsing.RegexMany @"\d+" [a;b;c;d] -> 
        {Registers.a = int a;b=int b;c=int c;d=int d}
    | _ -> failwith "Could not parse register"

let private partToInstruction (line:string) =
    match line with
    | Parsing.RegexMany @"\d+" [opNo;a;b;c] -> 
        {Instruction.opNo=int opNo;a=int a;b=int b;c=int c}
    | _ -> failwith "Could not parse instruction"

let execute = fun d ->
    let nl = System.Environment.NewLine
    let splitString = nl+nl+nl
    let parts = d |> splitDataSet splitString

    let first = parts |> List.head |> splitDataSet (nl+nl) 

    let data = first 
            |> List.map (fun s -> Parsing.splitLines s |> Array.ofSeq) 
            |> List.map (fun ar -> 
                    let before = ar.[0] |> parseToRegisters
                    let action = ar.[1] |> partToInstruction
                    let after  = ar.[2] |> parseToRegisters
                    (before,action,after))

    let r = data |> List.map (fun (rS,i,rT) -> passing OpFunctions (toValues i) rS rT |> countOfTrue)
    let results = r |> List.filter (fun c -> c > 2) |> List.length

    printfn "Day 16 - Part 1: Count of %i" results 
    
    let sets = data |> List.map (fun (rS,i,rT) -> (i.opNo, passing OpFunctions (toValues i) rS rT)) |> List.groupBy fst
    let results = sets |> List.map (fun (_,l) -> l |> flattenPasses)
    let d = results |> determineOps OpFunctions

    let second = parts.Item 1 |> Parsing.splitLines |> Seq.map partToInstruction |> List.ofSeq
    let r = runProgram d second

    printfn "Day 16 - Part 2: Value in register 0 is %i" r.a

let testSet = @"
Before: [3, 2, 1, 1]
9 2 1 2
After:  [3, 2, 2, 1]
"

let dataSet = @"
Before: [3, 3, 2, 3]
3 1 2 2
After:  [3, 3, 2, 3]

Before: [1, 3, 0, 1]
12 0 2 3
After:  [1, 3, 0, 0]

Before: [0, 3, 2, 0]
14 2 3 0
After:  [1, 3, 2, 0]

Before: [2, 3, 3, 3]
10 0 3 0
After:  [2, 3, 3, 3]

Before: [0, 1, 2, 0]
7 1 2 3
After:  [0, 1, 2, 0]

Before: [3, 1, 2, 0]
7 1 2 2
After:  [3, 1, 0, 0]

Before: [1, 2, 1, 3]
6 2 2 2
After:  [1, 2, 2, 3]

Before: [1, 3, 2, 3]
3 3 2 3
After:  [1, 3, 2, 2]

Before: [0, 1, 2, 0]
8 0 0 1
After:  [0, 0, 2, 0]

Before: [1, 2, 3, 1]
0 2 3 2
After:  [1, 2, 1, 1]

Before: [3, 2, 2, 1]
9 3 2 3
After:  [3, 2, 2, 1]

Before: [1, 2, 2, 3]
4 0 2 0
After:  [0, 2, 2, 3]

Before: [1, 3, 2, 0]
3 1 2 1
After:  [1, 2, 2, 0]

Before: [3, 0, 0, 3]
1 1 0 1
After:  [3, 0, 0, 3]

Before: [1, 1, 2, 0]
4 0 2 2
After:  [1, 1, 0, 0]

Before: [2, 0, 1, 3]
1 1 0 2
After:  [2, 0, 0, 3]

Before: [3, 2, 2, 1]
9 3 2 2
After:  [3, 2, 1, 1]

Before: [2, 2, 2, 3]
3 3 2 3
After:  [2, 2, 2, 2]

Before: [0, 3, 2, 2]
5 3 1 2
After:  [0, 3, 0, 2]

Before: [2, 0, 2, 1]
9 3 2 3
After:  [2, 0, 2, 1]

Before: [0, 3, 2, 2]
5 3 1 0
After:  [0, 3, 2, 2]

Before: [3, 0, 0, 2]
1 1 0 3
After:  [3, 0, 0, 0]

Before: [1, 3, 2, 2]
11 2 1 1
After:  [1, 0, 2, 2]

Before: [1, 3, 2, 2]
11 2 1 2
After:  [1, 3, 0, 2]

Before: [0, 0, 3, 1]
0 2 3 1
After:  [0, 1, 3, 1]

Before: [2, 2, 2, 1]
9 3 2 1
After:  [2, 1, 2, 1]

Before: [0, 0, 2, 1]
2 2 2 1
After:  [0, 4, 2, 1]

Before: [0, 0, 2, 1]
2 2 2 0
After:  [4, 0, 2, 1]

Before: [2, 3, 0, 2]
15 1 0 2
After:  [2, 3, 2, 2]

Before: [1, 0, 1, 2]
6 2 2 2
After:  [1, 0, 2, 2]

Before: [2, 3, 2, 0]
2 2 2 0
After:  [4, 3, 2, 0]

Before: [1, 3, 3, 2]
5 3 1 3
After:  [1, 3, 3, 0]

Before: [1, 3, 0, 1]
12 0 2 2
After:  [1, 3, 0, 1]

Before: [3, 3, 1, 2]
15 1 3 3
After:  [3, 3, 1, 2]

Before: [0, 1, 1, 0]
13 0 2 1
After:  [0, 0, 1, 0]

Before: [0, 3, 2, 1]
14 2 3 1
After:  [0, 1, 2, 1]

Before: [3, 1, 2, 2]
7 1 2 2
After:  [3, 1, 0, 2]

Before: [2, 1, 2, 3]
2 2 2 1
After:  [2, 4, 2, 3]

Before: [0, 0, 3, 0]
13 0 2 2
After:  [0, 0, 0, 0]

Before: [0, 0, 3, 2]
8 0 0 1
After:  [0, 0, 3, 2]

Before: [2, 2, 2, 2]
2 3 2 3
After:  [2, 2, 2, 4]

Before: [3, 3, 1, 3]
10 2 3 2
After:  [3, 3, 1, 3]

Before: [1, 0, 3, 1]
0 2 3 0
After:  [1, 0, 3, 1]

Before: [3, 0, 1, 2]
15 0 3 1
After:  [3, 2, 1, 2]

Before: [0, 2, 1, 1]
6 2 2 1
After:  [0, 2, 1, 1]

Before: [0, 0, 2, 1]
9 3 2 2
After:  [0, 0, 1, 1]

Before: [0, 2, 1, 2]
13 0 3 0
After:  [0, 2, 1, 2]

Before: [1, 0, 2, 3]
3 3 2 3
After:  [1, 0, 2, 2]

Before: [1, 3, 2, 1]
14 2 3 2
After:  [1, 3, 1, 1]

Before: [0, 0, 2, 1]
9 3 2 0
After:  [1, 0, 2, 1]

Before: [0, 3, 2, 0]
8 0 0 0
After:  [0, 3, 2, 0]

Before: [2, 2, 2, 0]
14 2 3 3
After:  [2, 2, 2, 1]

Before: [3, 3, 2, 3]
11 2 1 2
After:  [3, 3, 0, 3]

Before: [1, 3, 1, 2]
5 3 1 0
After:  [0, 3, 1, 2]

Before: [0, 1, 2, 0]
13 0 1 3
After:  [0, 1, 2, 0]

Before: [1, 3, 1, 2]
5 3 1 3
After:  [1, 3, 1, 0]

Before: [1, 0, 2, 3]
2 2 2 0
After:  [4, 0, 2, 3]

Before: [0, 3, 2, 1]
14 2 3 3
After:  [0, 3, 2, 1]

Before: [0, 3, 2, 0]
11 2 1 2
After:  [0, 3, 0, 0]

Before: [1, 3, 2, 3]
3 1 2 1
After:  [1, 2, 2, 3]

Before: [3, 1, 2, 0]
7 1 2 1
After:  [3, 0, 2, 0]

Before: [0, 1, 0, 3]
13 0 3 0
After:  [0, 1, 0, 3]

Before: [0, 3, 2, 1]
11 2 1 3
After:  [0, 3, 2, 0]

Before: [0, 2, 2, 1]
9 3 2 0
After:  [1, 2, 2, 1]

Before: [3, 1, 2, 1]
9 3 2 1
After:  [3, 1, 2, 1]

Before: [3, 2, 0, 2]
1 2 0 3
After:  [3, 2, 0, 0]

Before: [3, 3, 3, 2]
5 3 1 2
After:  [3, 3, 0, 2]

Before: [3, 0, 1, 3]
1 1 0 2
After:  [3, 0, 0, 3]

Before: [1, 1, 2, 2]
7 1 2 0
After:  [0, 1, 2, 2]

Before: [3, 1, 2, 2]
7 1 2 1
After:  [3, 0, 2, 2]

Before: [3, 1, 0, 1]
1 2 0 3
After:  [3, 1, 0, 0]

Before: [1, 2, 2, 2]
4 0 2 3
After:  [1, 2, 2, 0]

Before: [1, 2, 0, 0]
12 0 2 1
After:  [1, 0, 0, 0]

Before: [0, 2, 2, 1]
2 1 2 3
After:  [0, 2, 2, 4]

Before: [0, 0, 3, 1]
13 0 3 1
After:  [0, 0, 3, 1]

Before: [0, 0, 2, 1]
14 2 3 3
After:  [0, 0, 2, 1]

Before: [2, 3, 2, 1]
11 2 1 1
After:  [2, 0, 2, 1]

Before: [2, 0, 1, 1]
1 1 0 2
After:  [2, 0, 0, 1]

Before: [2, 3, 1, 2]
6 2 2 0
After:  [2, 3, 1, 2]

Before: [2, 3, 2, 2]
11 2 1 0
After:  [0, 3, 2, 2]

Before: [3, 2, 2, 0]
2 1 2 2
After:  [3, 2, 4, 0]

Before: [0, 3, 1, 3]
13 0 1 2
After:  [0, 3, 0, 3]

Before: [0, 3, 2, 1]
11 2 1 1
After:  [0, 0, 2, 1]

Before: [2, 3, 2, 2]
5 3 1 2
After:  [2, 3, 0, 2]

Before: [2, 3, 2, 1]
15 1 0 1
After:  [2, 2, 2, 1]

Before: [2, 3, 2, 3]
11 2 1 2
After:  [2, 3, 0, 3]

Before: [0, 1, 2, 3]
7 1 2 1
After:  [0, 0, 2, 3]

Before: [0, 1, 3, 2]
13 0 2 2
After:  [0, 1, 0, 2]

Before: [3, 3, 2, 2]
3 0 2 3
After:  [3, 3, 2, 2]

Before: [2, 0, 2, 1]
9 3 2 2
After:  [2, 0, 1, 1]

Before: [0, 0, 2, 0]
14 2 3 0
After:  [1, 0, 2, 0]

Before: [3, 1, 2, 1]
7 1 2 1
After:  [3, 0, 2, 1]

Before: [0, 2, 2, 1]
14 2 3 1
After:  [0, 1, 2, 1]

Before: [1, 2, 2, 3]
10 1 3 3
After:  [1, 2, 2, 2]

Before: [3, 1, 2, 3]
3 3 2 0
After:  [2, 1, 2, 3]

Before: [0, 2, 2, 2]
8 0 0 2
After:  [0, 2, 0, 2]

Before: [0, 2, 2, 3]
8 0 0 3
After:  [0, 2, 2, 0]

Before: [0, 1, 0, 2]
8 0 0 2
After:  [0, 1, 0, 2]

Before: [1, 3, 2, 1]
9 3 2 3
After:  [1, 3, 2, 1]

Before: [3, 0, 2, 1]
14 2 3 2
After:  [3, 0, 1, 1]

Before: [1, 0, 2, 1]
2 2 2 2
After:  [1, 0, 4, 1]

Before: [0, 3, 0, 2]
5 3 1 0
After:  [0, 3, 0, 2]

Before: [3, 1, 2, 3]
7 1 2 1
After:  [3, 0, 2, 3]

Before: [2, 3, 2, 2]
11 2 1 1
After:  [2, 0, 2, 2]

Before: [3, 0, 2, 2]
3 0 2 0
After:  [2, 0, 2, 2]

Before: [1, 2, 2, 0]
4 0 2 0
After:  [0, 2, 2, 0]

Before: [1, 3, 0, 2]
5 3 1 0
After:  [0, 3, 0, 2]

Before: [1, 1, 2, 2]
7 1 2 2
After:  [1, 1, 0, 2]

Before: [0, 2, 0, 0]
8 0 0 1
After:  [0, 0, 0, 0]

Before: [3, 3, 2, 1]
11 2 1 0
After:  [0, 3, 2, 1]

Before: [3, 3, 1, 2]
5 3 1 0
After:  [0, 3, 1, 2]

Before: [3, 2, 3, 3]
15 0 1 0
After:  [2, 2, 3, 3]

Before: [1, 1, 2, 3]
3 3 2 0
After:  [2, 1, 2, 3]

Before: [2, 0, 2, 1]
1 1 0 1
After:  [2, 0, 2, 1]

Before: [1, 0, 0, 1]
12 0 2 3
After:  [1, 0, 0, 0]

Before: [3, 2, 2, 0]
3 0 2 0
After:  [2, 2, 2, 0]

Before: [1, 1, 0, 0]
12 0 2 2
After:  [1, 1, 0, 0]

Before: [1, 3, 3, 2]
5 3 1 1
After:  [1, 0, 3, 2]

Before: [2, 1, 2, 1]
9 3 2 1
After:  [2, 1, 2, 1]

Before: [2, 0, 2, 3]
15 3 0 2
After:  [2, 0, 2, 3]

Before: [2, 0, 2, 1]
9 3 2 1
After:  [2, 1, 2, 1]

Before: [3, 3, 2, 0]
2 2 2 1
After:  [3, 4, 2, 0]

Before: [3, 2, 2, 2]
15 0 3 3
After:  [3, 2, 2, 2]

Before: [0, 2, 3, 1]
13 0 3 3
After:  [0, 2, 3, 0]

Before: [1, 1, 2, 0]
4 0 2 0
After:  [0, 1, 2, 0]

Before: [2, 2, 2, 0]
0 2 1 3
After:  [2, 2, 2, 0]

Before: [0, 3, 2, 2]
11 2 1 2
After:  [0, 3, 0, 2]

Before: [1, 3, 2, 1]
4 0 2 3
After:  [1, 3, 2, 0]

Before: [3, 0, 2, 0]
1 1 0 1
After:  [3, 0, 2, 0]

Before: [3, 1, 1, 2]
15 0 3 1
After:  [3, 2, 1, 2]

Before: [1, 0, 2, 1]
4 0 2 1
After:  [1, 0, 2, 1]

Before: [2, 0, 1, 0]
6 2 2 1
After:  [2, 2, 1, 0]

Before: [3, 1, 2, 3]
3 0 2 3
After:  [3, 1, 2, 2]

Before: [3, 0, 3, 1]
0 2 3 3
After:  [3, 0, 3, 1]

Before: [0, 2, 2, 1]
2 1 2 1
After:  [0, 4, 2, 1]

Before: [1, 3, 1, 2]
6 2 2 2
After:  [1, 3, 2, 2]

Before: [3, 1, 2, 0]
14 2 3 2
After:  [3, 1, 1, 0]

Before: [1, 0, 2, 2]
4 0 2 1
After:  [1, 0, 2, 2]

Before: [3, 3, 2, 1]
9 3 2 2
After:  [3, 3, 1, 1]

Before: [1, 3, 2, 2]
5 3 1 1
After:  [1, 0, 2, 2]

Before: [1, 2, 2, 1]
4 0 2 0
After:  [0, 2, 2, 1]

Before: [0, 2, 2, 2]
0 2 1 0
After:  [0, 2, 2, 2]

Before: [0, 0, 3, 3]
8 0 0 0
After:  [0, 0, 3, 3]

Before: [1, 2, 1, 2]
6 2 2 3
After:  [1, 2, 1, 2]

Before: [1, 2, 0, 0]
12 0 2 0
After:  [0, 2, 0, 0]

Before: [2, 3, 2, 1]
11 2 1 0
After:  [0, 3, 2, 1]

Before: [3, 2, 2, 3]
15 3 1 2
After:  [3, 2, 2, 3]

Before: [3, 1, 0, 2]
1 2 0 0
After:  [0, 1, 0, 2]

Before: [0, 2, 1, 0]
8 0 0 2
After:  [0, 2, 0, 0]

Before: [1, 2, 2, 2]
2 2 2 1
After:  [1, 4, 2, 2]

Before: [2, 1, 2, 3]
15 3 0 1
After:  [2, 2, 2, 3]

Before: [2, 0, 2, 3]
1 1 0 3
After:  [2, 0, 2, 0]

Before: [0, 2, 2, 0]
0 2 1 0
After:  [0, 2, 2, 0]

Before: [1, 2, 2, 1]
14 2 3 1
After:  [1, 1, 2, 1]

Before: [1, 1, 2, 3]
7 1 2 2
After:  [1, 1, 0, 3]

Before: [0, 1, 1, 0]
6 2 2 0
After:  [2, 1, 1, 0]

Before: [3, 3, 0, 2]
5 3 1 3
After:  [3, 3, 0, 0]

Before: [3, 3, 1, 3]
6 2 2 3
After:  [3, 3, 1, 2]

Before: [2, 3, 2, 0]
3 1 2 0
After:  [2, 3, 2, 0]

Before: [0, 3, 3, 1]
8 0 0 0
After:  [0, 3, 3, 1]

Before: [0, 2, 1, 1]
6 2 2 2
After:  [0, 2, 2, 1]

Before: [0, 3, 2, 3]
3 3 2 0
After:  [2, 3, 2, 3]

Before: [2, 1, 2, 0]
7 1 2 0
After:  [0, 1, 2, 0]

Before: [1, 1, 2, 2]
4 0 2 0
After:  [0, 1, 2, 2]

Before: [1, 3, 3, 1]
0 2 3 3
After:  [1, 3, 3, 1]

Before: [3, 1, 3, 1]
0 2 3 1
After:  [3, 1, 3, 1]

Before: [1, 1, 0, 2]
12 0 2 1
After:  [1, 0, 0, 2]

Before: [0, 3, 3, 3]
8 0 0 3
After:  [0, 3, 3, 0]

Before: [2, 0, 3, 0]
1 1 0 3
After:  [2, 0, 3, 0]

Before: [2, 1, 2, 1]
14 2 3 0
After:  [1, 1, 2, 1]

Before: [2, 1, 2, 0]
7 1 2 1
After:  [2, 0, 2, 0]

Before: [2, 3, 3, 3]
10 0 3 1
After:  [2, 2, 3, 3]

Before: [3, 2, 0, 3]
15 0 1 0
After:  [2, 2, 0, 3]

Before: [2, 2, 2, 0]
0 2 1 0
After:  [0, 2, 2, 0]

Before: [2, 0, 1, 3]
1 1 0 3
After:  [2, 0, 1, 0]

Before: [3, 1, 0, 2]
1 2 0 1
After:  [3, 0, 0, 2]

Before: [3, 0, 2, 1]
9 3 2 1
After:  [3, 1, 2, 1]

Before: [3, 3, 2, 0]
3 0 2 3
After:  [3, 3, 2, 2]

Before: [2, 0, 2, 1]
2 2 2 0
After:  [4, 0, 2, 1]

Before: [0, 1, 0, 1]
8 0 0 2
After:  [0, 1, 0, 1]

Before: [3, 0, 2, 0]
2 2 2 1
After:  [3, 4, 2, 0]

Before: [1, 3, 2, 2]
11 2 1 0
After:  [0, 3, 2, 2]

Before: [1, 0, 0, 0]
12 0 2 2
After:  [1, 0, 0, 0]

Before: [3, 1, 2, 2]
2 3 2 2
After:  [3, 1, 4, 2]

Before: [0, 0, 2, 2]
2 3 2 3
After:  [0, 0, 2, 4]

Before: [2, 3, 2, 3]
3 1 2 3
After:  [2, 3, 2, 2]

Before: [2, 3, 1, 3]
10 2 3 3
After:  [2, 3, 1, 1]

Before: [3, 2, 2, 3]
3 3 2 0
After:  [2, 2, 2, 3]

Before: [2, 0, 1, 0]
6 2 2 2
After:  [2, 0, 2, 0]

Before: [1, 2, 2, 0]
4 0 2 3
After:  [1, 2, 2, 0]

Before: [3, 3, 1, 2]
5 3 1 3
After:  [3, 3, 1, 0]

Before: [3, 2, 2, 1]
9 3 2 1
After:  [3, 1, 2, 1]

Before: [2, 0, 1, 3]
6 2 2 1
After:  [2, 2, 1, 3]

Before: [0, 1, 3, 2]
8 0 0 3
After:  [0, 1, 3, 0]

Before: [1, 3, 0, 2]
12 0 2 0
After:  [0, 3, 0, 2]

Before: [3, 0, 3, 1]
0 2 3 1
After:  [3, 1, 3, 1]

Before: [0, 3, 3, 2]
5 3 1 3
After:  [0, 3, 3, 0]

Before: [0, 0, 3, 2]
8 0 0 2
After:  [0, 0, 0, 2]

Before: [3, 2, 2, 3]
0 2 1 0
After:  [0, 2, 2, 3]

Before: [2, 3, 2, 0]
2 0 2 1
After:  [2, 4, 2, 0]

Before: [0, 1, 1, 2]
8 0 0 1
After:  [0, 0, 1, 2]

Before: [3, 0, 2, 1]
3 0 2 2
After:  [3, 0, 2, 1]

Before: [3, 2, 2, 3]
15 3 1 1
After:  [3, 2, 2, 3]

Before: [1, 2, 2, 0]
0 2 1 3
After:  [1, 2, 2, 0]

Before: [3, 2, 1, 1]
6 2 2 3
After:  [3, 2, 1, 2]

Before: [0, 0, 2, 1]
13 0 2 0
After:  [0, 0, 2, 1]

Before: [0, 0, 1, 1]
6 2 2 0
After:  [2, 0, 1, 1]

Before: [0, 1, 3, 3]
13 0 2 2
After:  [0, 1, 0, 3]

Before: [2, 2, 0, 3]
10 1 3 0
After:  [2, 2, 0, 3]

Before: [2, 2, 2, 3]
2 1 2 1
After:  [2, 4, 2, 3]

Before: [2, 2, 2, 0]
14 2 3 0
After:  [1, 2, 2, 0]

Before: [0, 3, 3, 1]
8 0 0 3
After:  [0, 3, 3, 0]

Before: [2, 2, 2, 1]
9 3 2 3
After:  [2, 2, 2, 1]

Before: [1, 3, 1, 2]
6 2 2 3
After:  [1, 3, 1, 2]

Before: [1, 1, 1, 1]
6 2 2 3
After:  [1, 1, 1, 2]

Before: [0, 2, 2, 1]
2 2 2 3
After:  [0, 2, 2, 4]

Before: [1, 3, 2, 2]
4 0 2 0
After:  [0, 3, 2, 2]

Before: [1, 3, 1, 3]
6 2 2 3
After:  [1, 3, 1, 2]

Before: [0, 1, 1, 2]
13 0 2 1
After:  [0, 0, 1, 2]

Before: [3, 2, 2, 3]
3 0 2 1
After:  [3, 2, 2, 3]

Before: [1, 3, 3, 3]
10 0 3 3
After:  [1, 3, 3, 1]

Before: [0, 0, 1, 0]
6 2 2 1
After:  [0, 2, 1, 0]

Before: [0, 3, 3, 1]
8 0 0 2
After:  [0, 3, 0, 1]

Before: [2, 1, 0, 3]
15 3 0 1
After:  [2, 2, 0, 3]

Before: [1, 0, 0, 3]
12 0 2 0
After:  [0, 0, 0, 3]

Before: [1, 0, 0, 3]
12 0 2 2
After:  [1, 0, 0, 3]

Before: [2, 3, 2, 2]
5 3 1 1
After:  [2, 0, 2, 2]

Before: [1, 0, 0, 2]
12 0 2 0
After:  [0, 0, 0, 2]

Before: [0, 2, 1, 1]
6 2 2 0
After:  [2, 2, 1, 1]

Before: [0, 0, 1, 2]
8 0 0 2
After:  [0, 0, 0, 2]

Before: [0, 3, 2, 3]
13 0 1 2
After:  [0, 3, 0, 3]

Before: [3, 1, 2, 1]
9 3 2 0
After:  [1, 1, 2, 1]

Before: [1, 3, 2, 3]
4 0 2 1
After:  [1, 0, 2, 3]

Before: [1, 0, 0, 2]
12 0 2 3
After:  [1, 0, 0, 0]

Before: [2, 2, 0, 3]
15 3 1 2
After:  [2, 2, 2, 3]

Before: [0, 0, 3, 2]
13 0 3 1
After:  [0, 0, 3, 2]

Before: [1, 0, 2, 2]
4 0 2 2
After:  [1, 0, 0, 2]

Before: [1, 2, 1, 1]
6 2 2 3
After:  [1, 2, 1, 2]

Before: [1, 1, 2, 1]
4 0 2 1
After:  [1, 0, 2, 1]

Before: [1, 3, 0, 2]
5 3 1 2
After:  [1, 3, 0, 2]

Before: [3, 2, 0, 1]
1 2 0 3
After:  [3, 2, 0, 0]

Before: [3, 2, 2, 0]
14 2 3 3
After:  [3, 2, 2, 1]

Before: [2, 1, 2, 3]
15 3 0 2
After:  [2, 1, 2, 3]

Before: [2, 3, 2, 0]
14 2 3 2
After:  [2, 3, 1, 0]

Before: [0, 3, 3, 2]
5 3 1 2
After:  [0, 3, 0, 2]

Before: [0, 1, 2, 0]
7 1 2 1
After:  [0, 0, 2, 0]

Before: [3, 3, 2, 0]
3 1 2 2
After:  [3, 3, 2, 0]

Before: [2, 1, 2, 3]
7 1 2 0
After:  [0, 1, 2, 3]

Before: [1, 2, 2, 1]
4 0 2 2
After:  [1, 2, 0, 1]

Before: [0, 2, 3, 0]
8 0 0 2
After:  [0, 2, 0, 0]

Before: [2, 3, 2, 0]
11 2 1 3
After:  [2, 3, 2, 0]

Before: [2, 2, 2, 0]
2 0 2 0
After:  [4, 2, 2, 0]

Before: [1, 0, 2, 2]
2 3 2 1
After:  [1, 4, 2, 2]

Before: [0, 3, 1, 2]
13 0 2 2
After:  [0, 3, 0, 2]

Before: [1, 1, 2, 1]
4 0 2 0
After:  [0, 1, 2, 1]

Before: [0, 2, 1, 2]
8 0 0 2
After:  [0, 2, 0, 2]

Before: [3, 0, 2, 1]
9 3 2 0
After:  [1, 0, 2, 1]

Before: [1, 2, 2, 1]
4 0 2 1
After:  [1, 0, 2, 1]

Before: [0, 1, 2, 1]
14 2 3 2
After:  [0, 1, 1, 1]

Before: [1, 0, 2, 1]
2 2 2 1
After:  [1, 4, 2, 1]

Before: [1, 0, 2, 3]
4 0 2 1
After:  [1, 0, 2, 3]

Before: [3, 0, 0, 2]
15 0 3 3
After:  [3, 0, 0, 2]

Before: [2, 2, 2, 1]
9 3 2 0
After:  [1, 2, 2, 1]

Before: [1, 3, 3, 2]
5 3 1 0
After:  [0, 3, 3, 2]

Before: [2, 0, 3, 1]
0 2 3 2
After:  [2, 0, 1, 1]

Before: [3, 3, 0, 2]
15 1 3 1
After:  [3, 2, 0, 2]

Before: [1, 3, 3, 2]
5 3 1 2
After:  [1, 3, 0, 2]

Before: [0, 3, 1, 3]
6 2 2 3
After:  [0, 3, 1, 2]

Before: [1, 1, 2, 0]
7 1 2 0
After:  [0, 1, 2, 0]

Before: [3, 1, 1, 3]
6 2 2 1
After:  [3, 2, 1, 3]

Before: [3, 3, 0, 2]
5 3 1 1
After:  [3, 0, 0, 2]

Before: [0, 1, 1, 1]
13 0 1 1
After:  [0, 0, 1, 1]

Before: [0, 3, 2, 2]
5 3 1 3
After:  [0, 3, 2, 0]

Before: [0, 1, 2, 1]
7 1 2 2
After:  [0, 1, 0, 1]

Before: [0, 0, 2, 1]
8 0 0 1
After:  [0, 0, 2, 1]

Before: [1, 1, 3, 3]
10 0 3 2
After:  [1, 1, 1, 3]

Before: [0, 0, 1, 1]
8 0 0 0
After:  [0, 0, 1, 1]

Before: [1, 3, 3, 2]
15 1 3 3
After:  [1, 3, 3, 2]

Before: [1, 3, 2, 0]
3 1 2 2
After:  [1, 3, 2, 0]

Before: [2, 0, 2, 1]
1 1 0 3
After:  [2, 0, 2, 0]

Before: [3, 0, 0, 2]
1 2 0 3
After:  [3, 0, 0, 0]

Before: [2, 3, 2, 2]
15 1 0 2
After:  [2, 3, 2, 2]

Before: [0, 2, 3, 1]
13 0 2 0
After:  [0, 2, 3, 1]

Before: [2, 0, 3, 2]
1 1 0 1
After:  [2, 0, 3, 2]

Before: [2, 1, 2, 1]
14 2 3 3
After:  [2, 1, 2, 1]

Before: [3, 2, 2, 3]
15 3 1 3
After:  [3, 2, 2, 2]

Before: [1, 0, 2, 1]
4 0 2 2
After:  [1, 0, 0, 1]

Before: [3, 2, 1, 2]
15 0 3 2
After:  [3, 2, 2, 2]

Before: [1, 1, 2, 1]
14 2 3 1
After:  [1, 1, 2, 1]

Before: [0, 3, 2, 1]
9 3 2 3
After:  [0, 3, 2, 1]

Before: [2, 3, 2, 3]
3 3 2 0
After:  [2, 3, 2, 3]

Before: [1, 1, 2, 0]
7 1 2 2
After:  [1, 1, 0, 0]

Before: [0, 0, 0, 1]
13 0 3 0
After:  [0, 0, 0, 1]

Before: [1, 1, 2, 3]
4 0 2 2
After:  [1, 1, 0, 3]

Before: [0, 3, 2, 0]
13 0 2 2
After:  [0, 3, 0, 0]

Before: [3, 1, 2, 1]
9 3 2 2
After:  [3, 1, 1, 1]

Before: [3, 0, 0, 1]
1 2 0 3
After:  [3, 0, 0, 0]

Before: [2, 2, 2, 3]
0 2 1 2
After:  [2, 2, 0, 3]

Before: [0, 0, 3, 1]
8 0 0 2
After:  [0, 0, 0, 1]

Before: [0, 3, 1, 2]
5 3 1 0
After:  [0, 3, 1, 2]

Before: [0, 1, 2, 0]
14 2 3 2
After:  [0, 1, 1, 0]

Before: [0, 3, 2, 3]
3 1 2 1
After:  [0, 2, 2, 3]

Before: [0, 3, 0, 2]
5 3 1 3
After:  [0, 3, 0, 0]

Before: [2, 3, 3, 2]
5 3 1 0
After:  [0, 3, 3, 2]

Before: [0, 0, 3, 1]
8 0 0 0
After:  [0, 0, 3, 1]

Before: [0, 2, 2, 0]
8 0 0 3
After:  [0, 2, 2, 0]

Before: [3, 1, 1, 3]
6 2 2 3
After:  [3, 1, 1, 2]

Before: [0, 3, 3, 0]
8 0 0 0
After:  [0, 3, 3, 0]

Before: [2, 2, 2, 3]
10 1 3 2
After:  [2, 2, 2, 3]

Before: [2, 3, 1, 3]
6 2 2 0
After:  [2, 3, 1, 3]

Before: [2, 3, 2, 3]
3 3 2 2
After:  [2, 3, 2, 3]

Before: [0, 1, 2, 3]
8 0 0 0
After:  [0, 1, 2, 3]

Before: [2, 2, 0, 3]
10 0 3 3
After:  [2, 2, 0, 2]

Before: [3, 0, 0, 3]
1 1 0 3
After:  [3, 0, 0, 0]

Before: [0, 2, 2, 1]
14 2 3 3
After:  [0, 2, 2, 1]

Before: [1, 3, 2, 2]
11 2 1 3
After:  [1, 3, 2, 0]

Before: [3, 2, 1, 2]
15 0 3 3
After:  [3, 2, 1, 2]

Before: [0, 2, 1, 3]
6 2 2 3
After:  [0, 2, 1, 2]

Before: [3, 0, 2, 2]
2 3 2 1
After:  [3, 4, 2, 2]

Before: [3, 1, 0, 0]
1 2 0 0
After:  [0, 1, 0, 0]

Before: [0, 3, 3, 1]
13 0 2 1
After:  [0, 0, 3, 1]

Before: [0, 3, 2, 1]
13 0 3 1
After:  [0, 0, 2, 1]

Before: [0, 3, 2, 1]
9 3 2 0
After:  [1, 3, 2, 1]

Before: [0, 0, 2, 2]
2 2 2 2
After:  [0, 0, 4, 2]

Before: [0, 2, 2, 3]
8 0 0 2
After:  [0, 2, 0, 3]

Before: [1, 2, 2, 2]
4 0 2 1
After:  [1, 0, 2, 2]

Before: [3, 0, 3, 2]
1 1 0 2
After:  [3, 0, 0, 2]

Before: [1, 3, 2, 1]
9 3 2 0
After:  [1, 3, 2, 1]

Before: [0, 1, 1, 2]
13 0 1 2
After:  [0, 1, 0, 2]

Before: [1, 3, 2, 0]
2 2 2 1
After:  [1, 4, 2, 0]

Before: [2, 1, 2, 1]
7 1 2 3
After:  [2, 1, 2, 0]

Before: [2, 0, 2, 3]
3 3 2 1
After:  [2, 2, 2, 3]

Before: [2, 1, 2, 3]
7 1 2 1
After:  [2, 0, 2, 3]

Before: [3, 2, 2, 1]
15 0 1 2
After:  [3, 2, 2, 1]

Before: [1, 2, 2, 3]
4 0 2 1
After:  [1, 0, 2, 3]

Before: [2, 1, 2, 0]
7 1 2 2
After:  [2, 1, 0, 0]

Before: [3, 0, 0, 3]
1 2 0 1
After:  [3, 0, 0, 3]

Before: [3, 2, 2, 0]
3 0 2 3
After:  [3, 2, 2, 2]

Before: [2, 3, 2, 2]
11 2 1 2
After:  [2, 3, 0, 2]

Before: [2, 3, 2, 0]
11 2 1 2
After:  [2, 3, 0, 0]

Before: [1, 1, 1, 3]
10 0 3 3
After:  [1, 1, 1, 1]

Before: [1, 2, 2, 1]
9 3 2 3
After:  [1, 2, 2, 1]

Before: [3, 3, 3, 2]
5 3 1 3
After:  [3, 3, 3, 0]

Before: [0, 2, 2, 1]
9 3 2 1
After:  [0, 1, 2, 1]

Before: [2, 2, 2, 3]
3 3 2 0
After:  [2, 2, 2, 3]

Before: [0, 1, 2, 3]
3 3 2 3
After:  [0, 1, 2, 2]

Before: [2, 2, 3, 1]
0 2 3 3
After:  [2, 2, 3, 1]

Before: [0, 2, 2, 1]
9 3 2 2
After:  [0, 2, 1, 1]

Before: [0, 2, 2, 2]
2 2 2 3
After:  [0, 2, 2, 4]

Before: [3, 0, 1, 1]
6 2 2 3
After:  [3, 0, 1, 2]

Before: [0, 3, 2, 3]
11 2 1 3
After:  [0, 3, 2, 0]

Before: [1, 0, 2, 3]
10 0 3 2
After:  [1, 0, 1, 3]

Before: [1, 1, 2, 1]
9 3 2 0
After:  [1, 1, 2, 1]

Before: [1, 3, 2, 1]
3 1 2 3
After:  [1, 3, 2, 2]

Before: [2, 0, 3, 1]
0 2 3 3
After:  [2, 0, 3, 1]

Before: [1, 0, 2, 0]
4 0 2 3
After:  [1, 0, 2, 0]

Before: [0, 3, 2, 3]
3 1 2 0
After:  [2, 3, 2, 3]

Before: [2, 2, 2, 3]
2 0 2 3
After:  [2, 2, 2, 4]

Before: [2, 3, 3, 3]
10 0 3 3
After:  [2, 3, 3, 2]

Before: [0, 3, 0, 2]
5 3 1 1
After:  [0, 0, 0, 2]

Before: [3, 1, 2, 3]
7 1 2 3
After:  [3, 1, 2, 0]

Before: [0, 0, 2, 3]
2 2 2 1
After:  [0, 4, 2, 3]

Before: [3, 0, 1, 1]
1 1 0 2
After:  [3, 0, 0, 1]

Before: [2, 0, 2, 0]
1 1 0 0
After:  [0, 0, 2, 0]

Before: [1, 0, 0, 0]
12 0 2 0
After:  [0, 0, 0, 0]

Before: [0, 2, 1, 3]
10 1 3 3
After:  [0, 2, 1, 2]

Before: [2, 0, 1, 3]
15 3 0 1
After:  [2, 2, 1, 3]

Before: [2, 3, 1, 2]
5 3 1 0
After:  [0, 3, 1, 2]

Before: [1, 3, 0, 2]
12 0 2 2
After:  [1, 3, 0, 2]

Before: [0, 3, 2, 2]
11 2 1 3
After:  [0, 3, 2, 0]

Before: [3, 0, 0, 1]
1 2 0 2
After:  [3, 0, 0, 1]

Before: [2, 0, 0, 0]
1 1 0 1
After:  [2, 0, 0, 0]

Before: [0, 2, 1, 2]
13 0 1 1
After:  [0, 0, 1, 2]

Before: [1, 1, 2, 0]
4 0 2 3
After:  [1, 1, 2, 0]

Before: [1, 1, 2, 0]
4 0 2 1
After:  [1, 0, 2, 0]

Before: [2, 1, 2, 3]
15 3 0 0
After:  [2, 1, 2, 3]

Before: [2, 0, 0, 1]
1 1 0 2
After:  [2, 0, 0, 1]

Before: [1, 1, 2, 2]
2 3 2 3
After:  [1, 1, 2, 4]

Before: [0, 2, 2, 2]
0 2 1 1
After:  [0, 0, 2, 2]

Before: [1, 2, 0, 2]
12 0 2 3
After:  [1, 2, 0, 0]

Before: [3, 2, 3, 1]
0 2 3 3
After:  [3, 2, 3, 1]

Before: [1, 0, 0, 3]
12 0 2 3
After:  [1, 0, 0, 0]

Before: [1, 2, 2, 0]
4 0 2 2
After:  [1, 2, 0, 0]

Before: [1, 3, 2, 0]
4 0 2 1
After:  [1, 0, 2, 0]

Before: [2, 3, 2, 1]
3 1 2 3
After:  [2, 3, 2, 2]

Before: [2, 1, 2, 0]
14 2 3 3
After:  [2, 1, 2, 1]

Before: [2, 3, 2, 2]
5 3 1 0
After:  [0, 3, 2, 2]

Before: [3, 3, 2, 2]
3 0 2 0
After:  [2, 3, 2, 2]

Before: [0, 3, 2, 3]
3 1 2 3
After:  [0, 3, 2, 2]

Before: [1, 3, 2, 1]
9 3 2 2
After:  [1, 3, 1, 1]

Before: [2, 1, 2, 3]
7 1 2 3
After:  [2, 1, 2, 0]

Before: [0, 3, 2, 1]
3 1 2 2
After:  [0, 3, 2, 1]

Before: [0, 2, 1, 0]
13 0 1 3
After:  [0, 2, 1, 0]

Before: [0, 3, 1, 0]
8 0 0 0
After:  [0, 3, 1, 0]

Before: [1, 0, 0, 3]
10 0 3 0
After:  [1, 0, 0, 3]

Before: [1, 1, 0, 0]
12 0 2 3
After:  [1, 1, 0, 0]

Before: [3, 3, 2, 2]
5 3 1 2
After:  [3, 3, 0, 2]

Before: [0, 3, 2, 3]
13 0 1 3
After:  [0, 3, 2, 0]

Before: [1, 3, 1, 3]
10 2 3 1
After:  [1, 1, 1, 3]

Before: [1, 2, 2, 2]
2 1 2 0
After:  [4, 2, 2, 2]

Before: [0, 3, 2, 3]
3 3 2 2
After:  [0, 3, 2, 3]

Before: [1, 2, 0, 3]
12 0 2 1
After:  [1, 0, 0, 3]

Before: [3, 0, 2, 3]
3 0 2 1
After:  [3, 2, 2, 3]

Before: [0, 1, 3, 0]
13 0 1 1
After:  [0, 0, 3, 0]

Before: [1, 2, 2, 2]
2 3 2 1
After:  [1, 4, 2, 2]

Before: [3, 2, 1, 2]
6 2 2 1
After:  [3, 2, 1, 2]

Before: [2, 3, 1, 3]
10 0 3 0
After:  [2, 3, 1, 3]

Before: [3, 3, 2, 1]
14 2 3 3
After:  [3, 3, 2, 1]

Before: [2, 3, 3, 1]
0 2 3 0
After:  [1, 3, 3, 1]

Before: [1, 1, 0, 2]
12 0 2 0
After:  [0, 1, 0, 2]

Before: [0, 3, 0, 3]
8 0 0 1
After:  [0, 0, 0, 3]

Before: [1, 2, 3, 1]
0 2 3 1
After:  [1, 1, 3, 1]

Before: [2, 1, 2, 0]
14 2 3 2
After:  [2, 1, 1, 0]

Before: [0, 1, 2, 3]
7 1 2 3
After:  [0, 1, 2, 0]

Before: [3, 3, 3, 2]
5 3 1 1
After:  [3, 0, 3, 2]

Before: [2, 3, 2, 3]
11 2 1 1
After:  [2, 0, 2, 3]

Before: [2, 0, 0, 2]
1 1 0 1
After:  [2, 0, 0, 2]

Before: [3, 1, 2, 0]
3 0 2 1
After:  [3, 2, 2, 0]

Before: [1, 1, 2, 1]
7 1 2 3
After:  [1, 1, 2, 0]

Before: [2, 3, 0, 2]
15 1 3 3
After:  [2, 3, 0, 2]

Before: [1, 3, 2, 0]
14 2 3 3
After:  [1, 3, 2, 1]

Before: [3, 3, 2, 0]
14 2 3 3
After:  [3, 3, 2, 1]

Before: [3, 3, 2, 2]
11 2 1 2
After:  [3, 3, 0, 2]

Before: [1, 2, 2, 3]
0 2 1 1
After:  [1, 0, 2, 3]

Before: [3, 2, 3, 3]
10 1 3 2
After:  [3, 2, 2, 3]

Before: [2, 1, 2, 2]
7 1 2 0
After:  [0, 1, 2, 2]

Before: [1, 1, 1, 1]
6 2 2 1
After:  [1, 2, 1, 1]

Before: [3, 1, 2, 2]
3 0 2 3
After:  [3, 1, 2, 2]

Before: [1, 3, 2, 0]
11 2 1 0
After:  [0, 3, 2, 0]

Before: [2, 3, 0, 0]
15 1 0 1
After:  [2, 2, 0, 0]

Before: [3, 3, 3, 2]
5 3 1 0
After:  [0, 3, 3, 2]

Before: [0, 3, 2, 0]
11 2 1 1
After:  [0, 0, 2, 0]

Before: [0, 3, 2, 2]
15 1 3 2
After:  [0, 3, 2, 2]

Before: [0, 1, 2, 1]
13 0 1 1
After:  [0, 0, 2, 1]

Before: [3, 1, 0, 2]
1 2 0 2
After:  [3, 1, 0, 2]

Before: [1, 2, 0, 3]
12 0 2 3
After:  [1, 2, 0, 0]

Before: [3, 3, 1, 0]
6 2 2 3
After:  [3, 3, 1, 2]

Before: [1, 1, 3, 1]
0 2 3 0
After:  [1, 1, 3, 1]

Before: [3, 1, 2, 1]
7 1 2 3
After:  [3, 1, 2, 0]

Before: [0, 1, 1, 3]
6 2 2 2
After:  [0, 1, 2, 3]

Before: [1, 2, 2, 3]
3 3 2 0
After:  [2, 2, 2, 3]

Before: [3, 0, 3, 0]
1 1 0 3
After:  [3, 0, 3, 0]

Before: [1, 0, 2, 1]
4 0 2 3
After:  [1, 0, 2, 0]

Before: [3, 0, 0, 2]
1 1 0 2
After:  [3, 0, 0, 2]

Before: [3, 0, 1, 0]
6 2 2 2
After:  [3, 0, 2, 0]

Before: [0, 2, 2, 3]
3 3 2 0
After:  [2, 2, 2, 3]

Before: [1, 3, 0, 3]
10 0 3 3
After:  [1, 3, 0, 1]

Before: [3, 1, 1, 3]
10 2 3 1
After:  [3, 1, 1, 3]

Before: [0, 1, 2, 3]
2 2 2 1
After:  [0, 4, 2, 3]

Before: [2, 3, 2, 1]
3 1 2 2
After:  [2, 3, 2, 1]

Before: [3, 3, 1, 2]
6 2 2 1
After:  [3, 2, 1, 2]

Before: [3, 0, 2, 3]
3 3 2 2
After:  [3, 0, 2, 3]

Before: [2, 1, 2, 3]
10 0 3 3
After:  [2, 1, 2, 2]

Before: [2, 2, 0, 3]
10 1 3 3
After:  [2, 2, 0, 2]

Before: [3, 3, 2, 0]
14 2 3 1
After:  [3, 1, 2, 0]

Before: [0, 3, 2, 0]
3 1 2 3
After:  [0, 3, 2, 2]

Before: [0, 2, 3, 0]
13 0 1 1
After:  [0, 0, 3, 0]

Before: [0, 2, 2, 2]
13 0 3 0
After:  [0, 2, 2, 2]

Before: [1, 1, 0, 1]
12 0 2 1
After:  [1, 0, 0, 1]

Before: [0, 3, 2, 0]
11 2 1 3
After:  [0, 3, 2, 0]

Before: [2, 1, 2, 1]
7 1 2 2
After:  [2, 1, 0, 1]

Before: [1, 3, 2, 0]
11 2 1 2
After:  [1, 3, 0, 0]

Before: [1, 3, 2, 0]
11 2 1 1
After:  [1, 0, 2, 0]

Before: [3, 0, 2, 1]
14 2 3 3
After:  [3, 0, 2, 1]

Before: [1, 0, 0, 1]
12 0 2 2
After:  [1, 0, 0, 1]

Before: [1, 1, 2, 2]
2 3 2 2
After:  [1, 1, 4, 2]

Before: [1, 1, 2, 1]
14 2 3 2
After:  [1, 1, 1, 1]

Before: [0, 3, 2, 0]
13 0 2 3
After:  [0, 3, 2, 0]

Before: [1, 3, 2, 3]
11 2 1 2
After:  [1, 3, 0, 3]

Before: [2, 1, 2, 3]
15 3 0 3
After:  [2, 1, 2, 2]

Before: [1, 3, 0, 3]
12 0 2 1
After:  [1, 0, 0, 3]

Before: [3, 3, 2, 1]
11 2 1 1
After:  [3, 0, 2, 1]

Before: [0, 3, 3, 3]
13 0 1 2
After:  [0, 3, 0, 3]

Before: [1, 1, 2, 3]
4 0 2 3
After:  [1, 1, 2, 0]

Before: [1, 2, 2, 0]
2 2 2 0
After:  [4, 2, 2, 0]

Before: [1, 0, 2, 2]
4 0 2 3
After:  [1, 0, 2, 0]

Before: [2, 3, 3, 2]
5 3 1 3
After:  [2, 3, 3, 0]

Before: [1, 0, 2, 1]
9 3 2 0
After:  [1, 0, 2, 1]

Before: [0, 3, 3, 0]
13 0 2 0
After:  [0, 3, 3, 0]

Before: [3, 1, 0, 3]
1 2 0 3
After:  [3, 1, 0, 0]

Before: [2, 2, 1, 3]
10 2 3 1
After:  [2, 1, 1, 3]

Before: [3, 1, 2, 0]
3 0 2 2
After:  [3, 1, 2, 0]

Before: [2, 0, 1, 0]
6 2 2 3
After:  [2, 0, 1, 2]

Before: [0, 2, 0, 0]
13 0 1 1
After:  [0, 0, 0, 0]

Before: [0, 2, 2, 1]
2 2 2 1
After:  [0, 4, 2, 1]

Before: [2, 3, 3, 1]
0 2 3 1
After:  [2, 1, 3, 1]

Before: [3, 1, 2, 1]
14 2 3 2
After:  [3, 1, 1, 1]

Before: [0, 2, 2, 3]
0 2 1 2
After:  [0, 2, 0, 3]

Before: [3, 1, 2, 1]
7 1 2 0
After:  [0, 1, 2, 1]

Before: [2, 1, 1, 3]
6 2 2 0
After:  [2, 1, 1, 3]

Before: [3, 3, 2, 1]
14 2 3 0
After:  [1, 3, 2, 1]

Before: [1, 2, 0, 2]
12 0 2 2
After:  [1, 2, 0, 2]

Before: [0, 1, 0, 3]
8 0 0 3
After:  [0, 1, 0, 0]

Before: [1, 3, 1, 2]
5 3 1 1
After:  [1, 0, 1, 2]

Before: [2, 1, 1, 0]
6 2 2 1
After:  [2, 2, 1, 0]

Before: [1, 2, 2, 3]
4 0 2 2
After:  [1, 2, 0, 3]

Before: [2, 3, 2, 1]
9 3 2 3
After:  [2, 3, 2, 1]

Before: [1, 0, 2, 0]
14 2 3 3
After:  [1, 0, 2, 1]

Before: [1, 0, 1, 3]
6 2 2 2
After:  [1, 0, 2, 3]

Before: [1, 1, 0, 3]
12 0 2 1
After:  [1, 0, 0, 3]

Before: [1, 1, 2, 2]
4 0 2 3
After:  [1, 1, 2, 0]

Before: [1, 0, 2, 1]
9 3 2 1
After:  [1, 1, 2, 1]

Before: [1, 3, 2, 1]
4 0 2 1
After:  [1, 0, 2, 1]

Before: [3, 1, 2, 0]
3 0 2 0
After:  [2, 1, 2, 0]

Before: [3, 3, 2, 0]
11 2 1 0
After:  [0, 3, 2, 0]

Before: [0, 3, 2, 2]
8 0 0 0
After:  [0, 3, 2, 2]

Before: [3, 2, 2, 0]
14 2 3 0
After:  [1, 2, 2, 0]

Before: [1, 2, 2, 3]
2 1 2 3
After:  [1, 2, 2, 4]

Before: [3, 2, 3, 0]
15 0 1 3
After:  [3, 2, 3, 2]

Before: [3, 1, 1, 2]
15 0 3 2
After:  [3, 1, 2, 2]

Before: [2, 3, 1, 3]
6 2 2 3
After:  [2, 3, 1, 2]

Before: [1, 1, 2, 3]
4 0 2 1
After:  [1, 0, 2, 3]

Before: [2, 2, 1, 0]
6 2 2 0
After:  [2, 2, 1, 0]

Before: [3, 1, 2, 1]
7 1 2 2
After:  [3, 1, 0, 1]

Before: [3, 3, 1, 2]
5 3 1 2
After:  [3, 3, 0, 2]

Before: [0, 3, 2, 1]
11 2 1 2
After:  [0, 3, 0, 1]

Before: [1, 3, 0, 2]
5 3 1 1
After:  [1, 0, 0, 2]

Before: [2, 1, 0, 3]
15 3 0 3
After:  [2, 1, 0, 2]

Before: [3, 0, 3, 1]
0 2 3 0
After:  [1, 0, 3, 1]

Before: [1, 1, 2, 3]
7 1 2 3
After:  [1, 1, 2, 0]

Before: [1, 3, 2, 3]
3 1 2 3
After:  [1, 3, 2, 2]

Before: [1, 2, 2, 1]
9 3 2 1
After:  [1, 1, 2, 1]

Before: [0, 0, 0, 3]
13 0 3 0
After:  [0, 0, 0, 3]

Before: [1, 0, 2, 3]
4 0 2 3
After:  [1, 0, 2, 0]

Before: [1, 3, 2, 2]
2 3 2 3
After:  [1, 3, 2, 4]

Before: [1, 0, 2, 1]
14 2 3 3
After:  [1, 0, 2, 1]

Before: [0, 1, 2, 1]
9 3 2 0
After:  [1, 1, 2, 1]

Before: [0, 3, 3, 2]
5 3 1 0
After:  [0, 3, 3, 2]

Before: [3, 2, 1, 0]
6 2 2 3
After:  [3, 2, 1, 2]

Before: [0, 3, 2, 3]
11 2 1 1
After:  [0, 0, 2, 3]

Before: [3, 0, 0, 1]
1 2 0 0
After:  [0, 0, 0, 1]

Before: [3, 2, 2, 3]
2 2 2 1
After:  [3, 4, 2, 3]

Before: [2, 0, 2, 3]
2 0 2 0
After:  [4, 0, 2, 3]

Before: [0, 2, 1, 3]
8 0 0 3
After:  [0, 2, 1, 0]

Before: [3, 2, 0, 0]
15 0 1 2
After:  [3, 2, 2, 0]

Before: [0, 1, 0, 2]
13 0 3 0
After:  [0, 1, 0, 2]

Before: [0, 3, 1, 3]
13 0 2 0
After:  [0, 3, 1, 3]

Before: [2, 3, 0, 2]
5 3 1 0
After:  [0, 3, 0, 2]

Before: [2, 3, 2, 1]
9 3 2 0
After:  [1, 3, 2, 1]

Before: [0, 3, 3, 1]
13 0 3 1
After:  [0, 0, 3, 1]

Before: [1, 3, 2, 0]
14 2 3 1
After:  [1, 1, 2, 0]

Before: [1, 3, 2, 1]
4 0 2 2
After:  [1, 3, 0, 1]

Before: [0, 0, 2, 1]
13 0 2 3
After:  [0, 0, 2, 0]

Before: [1, 1, 3, 1]
0 2 3 1
After:  [1, 1, 3, 1]

Before: [3, 3, 2, 2]
15 1 3 2
After:  [3, 3, 2, 2]

Before: [1, 3, 2, 2]
2 2 2 3
After:  [1, 3, 2, 4]

Before: [1, 3, 0, 3]
10 0 3 1
After:  [1, 1, 0, 3]

Before: [3, 0, 2, 1]
1 1 0 3
After:  [3, 0, 2, 0]

Before: [3, 1, 2, 1]
9 3 2 3
After:  [3, 1, 2, 1]

Before: [1, 2, 2, 3]
0 2 1 3
After:  [1, 2, 2, 0]

Before: [2, 2, 2, 1]
2 1 2 3
After:  [2, 2, 2, 4]

Before: [1, 3, 2, 1]
4 0 2 0
After:  [0, 3, 2, 1]

Before: [1, 1, 2, 1]
7 1 2 2
After:  [1, 1, 0, 1]

Before: [2, 3, 2, 2]
3 1 2 3
After:  [2, 3, 2, 2]

Before: [0, 1, 2, 1]
9 3 2 1
After:  [0, 1, 2, 1]

Before: [3, 0, 0, 3]
1 2 0 0
After:  [0, 0, 0, 3]

Before: [3, 0, 1, 1]
6 2 2 2
After:  [3, 0, 2, 1]

Before: [1, 1, 0, 2]
12 0 2 2
After:  [1, 1, 0, 2]

Before: [2, 3, 1, 3]
6 2 2 2
After:  [2, 3, 2, 3]

Before: [0, 3, 2, 3]
11 2 1 0
After:  [0, 3, 2, 3]

Before: [1, 2, 2, 3]
2 2 2 0
After:  [4, 2, 2, 3]

Before: [0, 2, 0, 2]
13 0 1 2
After:  [0, 2, 0, 2]

Before: [1, 3, 2, 3]
4 0 2 3
After:  [1, 3, 2, 0]

Before: [0, 2, 3, 3]
13 0 3 2
After:  [0, 2, 0, 3]

Before: [0, 3, 1, 2]
5 3 1 3
After:  [0, 3, 1, 0]

Before: [1, 0, 2, 1]
14 2 3 0
After:  [1, 0, 2, 1]

Before: [2, 2, 2, 0]
0 2 1 1
After:  [2, 0, 2, 0]

Before: [2, 3, 1, 2]
5 3 1 3
After:  [2, 3, 1, 0]

Before: [0, 2, 0, 1]
13 0 1 0
After:  [0, 2, 0, 1]

Before: [2, 2, 3, 3]
15 3 1 2
After:  [2, 2, 2, 3]

Before: [0, 1, 2, 3]
2 2 2 0
After:  [4, 1, 2, 3]

Before: [3, 1, 2, 0]
7 1 2 0
After:  [0, 1, 2, 0]

Before: [1, 2, 0, 2]
12 0 2 0
After:  [0, 2, 0, 2]

Before: [0, 0, 0, 0]
8 0 0 3
After:  [0, 0, 0, 0]

Before: [0, 2, 3, 2]
8 0 0 3
After:  [0, 2, 3, 0]

Before: [0, 1, 2, 2]
2 3 2 0
After:  [4, 1, 2, 2]

Before: [1, 3, 2, 0]
14 2 3 0
After:  [1, 3, 2, 0]

Before: [1, 2, 2, 3]
0 2 1 2
After:  [1, 2, 0, 3]

Before: [0, 2, 3, 3]
13 0 2 3
After:  [0, 2, 3, 0]

Before: [3, 3, 2, 0]
11 2 1 2
After:  [3, 3, 0, 0]

Before: [0, 3, 3, 2]
8 0 0 3
After:  [0, 3, 3, 0]

Before: [2, 1, 1, 0]
6 2 2 3
After:  [2, 1, 1, 2]

Before: [3, 3, 1, 1]
6 2 2 0
After:  [2, 3, 1, 1]

Before: [1, 1, 2, 3]
7 1 2 1
After:  [1, 0, 2, 3]

Before: [0, 1, 0, 1]
13 0 1 2
After:  [0, 1, 0, 1]

Before: [0, 1, 3, 1]
8 0 0 3
After:  [0, 1, 3, 0]

Before: [3, 3, 2, 0]
11 2 1 1
After:  [3, 0, 2, 0]

Before: [0, 1, 0, 0]
8 0 0 0
After:  [0, 1, 0, 0]

Before: [0, 2, 3, 0]
8 0 0 3
After:  [0, 2, 3, 0]

Before: [3, 0, 3, 2]
1 1 0 0
After:  [0, 0, 3, 2]

Before: [1, 0, 3, 1]
0 2 3 3
After:  [1, 0, 3, 1]

Before: [2, 3, 2, 3]
11 2 1 3
After:  [2, 3, 2, 0]

Before: [1, 1, 1, 1]
6 2 2 2
After:  [1, 1, 2, 1]

Before: [2, 3, 2, 2]
5 3 1 3
After:  [2, 3, 2, 0]

Before: [2, 3, 2, 1]
9 3 2 2
After:  [2, 3, 1, 1]

Before: [0, 2, 3, 1]
13 0 1 0
After:  [0, 2, 3, 1]

Before: [1, 0, 1, 3]
10 2 3 2
After:  [1, 0, 1, 3]

Before: [0, 3, 1, 2]
5 3 1 1
After:  [0, 0, 1, 2]

Before: [3, 0, 0, 3]
1 1 0 2
After:  [3, 0, 0, 3]

Before: [2, 3, 2, 1]
11 2 1 3
After:  [2, 3, 2, 0]

Before: [3, 2, 2, 1]
9 3 2 0
After:  [1, 2, 2, 1]

Before: [2, 2, 1, 2]
6 2 2 1
After:  [2, 2, 1, 2]

Before: [2, 3, 2, 3]
11 2 1 0
After:  [0, 3, 2, 3]

Before: [1, 3, 2, 0]
4 0 2 0
After:  [0, 3, 2, 0]

Before: [3, 1, 2, 2]
7 1 2 0
After:  [0, 1, 2, 2]

Before: [3, 1, 2, 3]
7 1 2 0
After:  [0, 1, 2, 3]

Before: [2, 3, 2, 3]
10 0 3 0
After:  [2, 3, 2, 3]

Before: [3, 2, 2, 3]
0 2 1 1
After:  [3, 0, 2, 3]

Before: [2, 0, 2, 0]
1 1 0 1
After:  [2, 0, 2, 0]

Before: [0, 3, 0, 2]
5 3 1 2
After:  [0, 3, 0, 2]

Before: [2, 2, 2, 1]
9 3 2 2
After:  [2, 2, 1, 1]

Before: [1, 1, 0, 1]
12 0 2 0
After:  [0, 1, 0, 1]

Before: [1, 0, 2, 1]
9 3 2 3
After:  [1, 0, 2, 1]

Before: [3, 2, 2, 3]
0 2 1 2
After:  [3, 2, 0, 3]

Before: [3, 0, 2, 2]
2 3 2 3
After:  [3, 0, 2, 4]

Before: [1, 0, 2, 2]
4 0 2 0
After:  [0, 0, 2, 2]

Before: [3, 3, 2, 2]
11 2 1 1
After:  [3, 0, 2, 2]

Before: [0, 1, 3, 1]
0 2 3 3
After:  [0, 1, 3, 1]

Before: [0, 0, 1, 0]
8 0 0 1
After:  [0, 0, 1, 0]

Before: [2, 0, 2, 1]
9 3 2 0
After:  [1, 0, 2, 1]

Before: [3, 0, 2, 2]
1 1 0 2
After:  [3, 0, 0, 2]

Before: [3, 3, 0, 1]
1 2 0 2
After:  [3, 3, 0, 1]

Before: [3, 2, 3, 3]
10 1 3 3
After:  [3, 2, 3, 2]

Before: [2, 3, 3, 1]
0 2 3 2
After:  [2, 3, 1, 1]

Before: [3, 1, 2, 2]
7 1 2 3
After:  [3, 1, 2, 0]

Before: [0, 3, 0, 3]
8 0 0 3
After:  [0, 3, 0, 0]

Before: [3, 0, 2, 2]
2 2 2 0
After:  [4, 0, 2, 2]

Before: [2, 3, 3, 3]
15 3 0 3
After:  [2, 3, 3, 2]

Before: [0, 2, 0, 1]
13 0 3 2
After:  [0, 2, 0, 1]

Before: [1, 2, 0, 3]
15 3 1 0
After:  [2, 2, 0, 3]

Before: [2, 2, 2, 2]
0 2 1 0
After:  [0, 2, 2, 2]

Before: [2, 0, 3, 1]
1 1 0 0
After:  [0, 0, 3, 1]

Before: [0, 0, 2, 1]
14 2 3 1
After:  [0, 1, 2, 1]

Before: [2, 2, 2, 1]
2 0 2 2
After:  [2, 2, 4, 1]

Before: [0, 3, 2, 1]
9 3 2 2
After:  [0, 3, 1, 1]

Before: [0, 3, 2, 2]
5 3 1 1
After:  [0, 0, 2, 2]

Before: [1, 2, 2, 2]
4 0 2 2
After:  [1, 2, 0, 2]

Before: [1, 1, 1, 3]
10 2 3 3
After:  [1, 1, 1, 1]

Before: [0, 2, 1, 1]
13 0 2 0
After:  [0, 2, 1, 1]

Before: [0, 3, 2, 2]
11 2 1 0
After:  [0, 3, 2, 2]

Before: [0, 2, 1, 2]
8 0 0 3
After:  [0, 2, 1, 0]

Before: [0, 3, 2, 2]
15 1 3 1
After:  [0, 2, 2, 2]

Before: [3, 0, 2, 0]
2 2 2 3
After:  [3, 0, 2, 4]

Before: [2, 2, 3, 3]
10 0 3 3
After:  [2, 2, 3, 2]

Before: [0, 2, 0, 3]
8 0 0 2
After:  [0, 2, 0, 3]

Before: [2, 2, 1, 3]
6 2 2 3
After:  [2, 2, 1, 2]

Before: [1, 2, 2, 0]
14 2 3 1
After:  [1, 1, 2, 0]

Before: [0, 3, 2, 1]
11 2 1 0
After:  [0, 3, 2, 1]

Before: [3, 3, 2, 1]
11 2 1 2
After:  [3, 3, 0, 1]

Before: [2, 3, 1, 3]
10 0 3 2
After:  [2, 3, 2, 3]

Before: [3, 1, 1, 1]
6 2 2 2
After:  [3, 1, 2, 1]

Before: [3, 2, 2, 0]
0 2 1 2
After:  [3, 2, 0, 0]

Before: [1, 3, 0, 2]
12 0 2 3
After:  [1, 3, 0, 0]

Before: [2, 3, 0, 2]
5 3 1 3
After:  [2, 3, 0, 0]

Before: [0, 1, 1, 0]
13 0 2 0
After:  [0, 1, 1, 0]

Before: [3, 3, 2, 1]
11 2 1 3
After:  [3, 3, 2, 0]

Before: [0, 1, 1, 3]
13 0 2 0
After:  [0, 1, 1, 3]

Before: [3, 3, 2, 3]
11 2 1 0
After:  [0, 3, 2, 3]

Before: [1, 1, 2, 1]
9 3 2 1
After:  [1, 1, 2, 1]

Before: [1, 1, 2, 1]
7 1 2 1
After:  [1, 0, 2, 1]

Before: [3, 2, 2, 3]
10 1 3 3
After:  [3, 2, 2, 2]

Before: [1, 2, 2, 3]
4 0 2 3
After:  [1, 2, 2, 0]

Before: [1, 3, 2, 3]
11 2 1 3
After:  [1, 3, 2, 0]

Before: [0, 0, 2, 1]
13 0 3 1
After:  [0, 0, 2, 1]

Before: [0, 1, 2, 0]
7 1 2 2
After:  [0, 1, 0, 0]

Before: [0, 3, 1, 3]
10 2 3 0
After:  [1, 3, 1, 3]

Before: [1, 1, 2, 2]
4 0 2 2
After:  [1, 1, 0, 2]

Before: [0, 1, 2, 3]
3 3 2 0
After:  [2, 1, 2, 3]

Before: [2, 0, 1, 2]
6 2 2 0
After:  [2, 0, 1, 2]

Before: [3, 0, 1, 2]
1 1 0 0
After:  [0, 0, 1, 2]

Before: [1, 2, 1, 3]
6 2 2 0
After:  [2, 2, 1, 3]

Before: [2, 3, 2, 0]
11 2 1 1
After:  [2, 0, 2, 0]

Before: [2, 3, 1, 1]
6 2 2 1
After:  [2, 2, 1, 1]

Before: [1, 0, 2, 1]
9 3 2 2
After:  [1, 0, 1, 1]

Before: [2, 3, 1, 3]
10 2 3 0
After:  [1, 3, 1, 3]

Before: [1, 2, 2, 1]
9 3 2 2
After:  [1, 2, 1, 1]

Before: [1, 3, 0, 1]
12 0 2 1
After:  [1, 0, 0, 1]

Before: [1, 2, 1, 3]
10 0 3 1
After:  [1, 1, 1, 3]

Before: [3, 0, 1, 3]
6 2 2 0
After:  [2, 0, 1, 3]

Before: [2, 3, 1, 3]
10 2 3 2
After:  [2, 3, 1, 3]

Before: [0, 1, 2, 1]
9 3 2 2
After:  [0, 1, 1, 1]

Before: [0, 3, 1, 2]
5 3 1 2
After:  [0, 3, 0, 2]

Before: [0, 2, 2, 1]
9 3 2 3
After:  [0, 2, 2, 1]

Before: [1, 3, 2, 2]
5 3 1 0
After:  [0, 3, 2, 2]

Before: [0, 1, 2, 0]
2 2 2 3
After:  [0, 1, 2, 4]

Before: [0, 3, 2, 3]
11 2 1 2
After:  [0, 3, 0, 3]

Before: [1, 3, 2, 0]
2 2 2 0
After:  [4, 3, 2, 0]

Before: [0, 1, 2, 3]
7 1 2 0
After:  [0, 1, 2, 3]

Before: [0, 1, 2, 1]
14 2 3 3
After:  [0, 1, 2, 1]

Before: [3, 3, 2, 0]
11 2 1 3
After:  [3, 3, 2, 0]

Before: [1, 3, 2, 1]
11 2 1 1
After:  [1, 0, 2, 1]

Before: [3, 3, 3, 2]
15 0 3 0
After:  [2, 3, 3, 2]

Before: [3, 3, 2, 2]
5 3 1 3
After:  [3, 3, 2, 0]

Before: [1, 0, 0, 2]
12 0 2 1
After:  [1, 0, 0, 2]

Before: [1, 0, 0, 3]
12 0 2 1
After:  [1, 0, 0, 3]

Before: [1, 3, 0, 2]
5 3 1 3
After:  [1, 3, 0, 0]

Before: [1, 1, 2, 3]
4 0 2 0
After:  [0, 1, 2, 3]

Before: [1, 0, 2, 0]
14 2 3 0
After:  [1, 0, 2, 0]

Before: [3, 3, 0, 3]
1 2 0 0
After:  [0, 3, 0, 3]

Before: [3, 3, 2, 1]
3 0 2 1
After:  [3, 2, 2, 1]

Before: [1, 3, 2, 0]
4 0 2 2
After:  [1, 3, 0, 0]

Before: [3, 2, 0, 1]
15 0 1 1
After:  [3, 2, 0, 1]

Before: [0, 3, 2, 3]
2 2 2 0
After:  [4, 3, 2, 3]

Before: [1, 0, 0, 0]
12 0 2 1
After:  [1, 0, 0, 0]

Before: [0, 0, 0, 1]
8 0 0 3
After:  [0, 0, 0, 0]

Before: [1, 3, 2, 3]
4 0 2 2
After:  [1, 3, 0, 3]

Before: [1, 3, 0, 0]
12 0 2 2
After:  [1, 3, 0, 0]

Before: [2, 2, 0, 3]
10 0 3 1
After:  [2, 2, 0, 3]

Before: [3, 0, 3, 3]
1 1 0 2
After:  [3, 0, 0, 3]

Before: [3, 3, 2, 2]
11 2 1 0
After:  [0, 3, 2, 2]

Before: [1, 2, 1, 3]
10 2 3 0
After:  [1, 2, 1, 3]

Before: [3, 3, 2, 3]
11 2 1 3
After:  [3, 3, 2, 0]

Before: [3, 2, 2, 0]
0 2 1 3
After:  [3, 2, 2, 0]

Before: [0, 2, 3, 1]
8 0 0 0
After:  [0, 2, 3, 1]

Before: [3, 0, 1, 2]
1 1 0 1
After:  [3, 0, 1, 2]

Before: [1, 1, 3, 1]
0 2 3 2
After:  [1, 1, 1, 1]

Before: [1, 2, 0, 0]
12 0 2 2
After:  [1, 2, 0, 0]

Before: [3, 0, 0, 0]
1 2 0 2
After:  [3, 0, 0, 0]

Before: [2, 1, 2, 3]
3 3 2 3
After:  [2, 1, 2, 2]

Before: [0, 2, 2, 0]
14 2 3 1
After:  [0, 1, 2, 0]

Before: [2, 3, 1, 2]
5 3 1 2
After:  [2, 3, 0, 2]

Before: [2, 2, 0, 3]
15 3 0 0
After:  [2, 2, 0, 3]

Before: [0, 2, 3, 2]
8 0 0 0
After:  [0, 2, 3, 2]

Before: [1, 3, 2, 1]
11 2 1 3
After:  [1, 3, 2, 0]

Before: [2, 1, 2, 1]
9 3 2 0
After:  [1, 1, 2, 1]

Before: [2, 2, 1, 3]
6 2 2 2
After:  [2, 2, 2, 3]

Before: [2, 3, 2, 1]
14 2 3 0
After:  [1, 3, 2, 1]

Before: [1, 3, 1, 3]
10 2 3 0
After:  [1, 3, 1, 3]

Before: [3, 3, 0, 2]
5 3 1 2
After:  [3, 3, 0, 2]

Before: [0, 3, 2, 3]
13 0 2 1
After:  [0, 0, 2, 3]

Before: [1, 0, 1, 1]
6 2 2 2
After:  [1, 0, 2, 1]

Before: [3, 3, 2, 2]
5 3 1 0
After:  [0, 3, 2, 2]

Before: [2, 1, 2, 1]
7 1 2 0
After:  [0, 1, 2, 1]

Before: [1, 1, 0, 0]
12 0 2 1
After:  [1, 0, 0, 0]

Before: [3, 3, 1, 0]
6 2 2 1
After:  [3, 2, 1, 0]

Before: [0, 1, 2, 0]
13 0 2 0
After:  [0, 1, 2, 0]

Before: [3, 3, 2, 0]
3 0 2 2
After:  [3, 3, 2, 0]

Before: [0, 1, 2, 1]
14 2 3 0
After:  [1, 1, 2, 1]

Before: [2, 3, 3, 0]
15 1 0 1
After:  [2, 2, 3, 0]

Before: [3, 2, 2, 2]
2 3 2 1
After:  [3, 4, 2, 2]

Before: [2, 2, 2, 3]
0 2 1 3
After:  [2, 2, 2, 0]

Before: [3, 0, 2, 2]
1 1 0 1
After:  [3, 0, 2, 2]

Before: [1, 2, 2, 0]
4 0 2 1
After:  [1, 0, 2, 0]

Before: [3, 3, 2, 0]
2 2 2 3
After:  [3, 3, 2, 4]

Before: [3, 2, 1, 3]
15 0 1 1
After:  [3, 2, 1, 3]

Before: [1, 3, 0, 0]
12 0 2 1
After:  [1, 0, 0, 0]

Before: [3, 3, 2, 3]
11 2 1 1
After:  [3, 0, 2, 3]

Before: [2, 0, 1, 1]
6 2 2 1
After:  [2, 2, 1, 1]

Before: [2, 1, 2, 1]
2 0 2 2
After:  [2, 1, 4, 1]

Before: [1, 0, 2, 3]
4 0 2 0
After:  [0, 0, 2, 3]

Before: [0, 3, 2, 1]
14 2 3 0
After:  [1, 3, 2, 1]

Before: [3, 2, 1, 3]
15 3 1 2
After:  [3, 2, 2, 3]

Before: [2, 0, 3, 3]
10 0 3 2
After:  [2, 0, 2, 3]

Before: [3, 0, 0, 2]
1 1 0 0
After:  [0, 0, 0, 2]

Before: [1, 3, 0, 3]
12 0 2 3
After:  [1, 3, 0, 0]

Before: [0, 1, 1, 2]
8 0 0 0
After:  [0, 1, 1, 2]

Before: [0, 3, 1, 1]
6 2 2 2
After:  [0, 3, 2, 1]

Before: [3, 3, 2, 2]
2 2 2 2
After:  [3, 3, 4, 2]

Before: [0, 3, 2, 3]
13 0 3 1
After:  [0, 0, 2, 3]

Before: [0, 1, 2, 2]
7 1 2 3
After:  [0, 1, 2, 0]

Before: [2, 2, 2, 1]
0 2 1 3
After:  [2, 2, 2, 0]

Before: [2, 3, 0, 2]
5 3 1 1
After:  [2, 0, 0, 2]

Before: [3, 2, 3, 0]
15 0 1 1
After:  [3, 2, 3, 0]

Before: [0, 3, 2, 2]
11 2 1 1
After:  [0, 0, 2, 2]

Before: [1, 1, 1, 3]
10 2 3 2
After:  [1, 1, 1, 3]

Before: [1, 0, 3, 1]
0 2 3 2
After:  [1, 0, 1, 1]

Before: [0, 3, 3, 1]
13 0 2 3
After:  [0, 3, 3, 0]

Before: [3, 2, 2, 1]
14 2 3 3
After:  [3, 2, 2, 1]

Before: [0, 0, 2, 3]
8 0 0 1
After:  [0, 0, 2, 3]

Before: [0, 3, 3, 3]
13 0 3 3
After:  [0, 3, 3, 0]

Before: [1, 3, 2, 3]
11 2 1 0
After:  [0, 3, 2, 3]

Before: [3, 0, 0, 1]
1 1 0 3
After:  [3, 0, 0, 0]

Before: [0, 3, 1, 1]
8 0 0 3
After:  [0, 3, 1, 0]

Before: [0, 0, 2, 3]
13 0 2 2
After:  [0, 0, 0, 3]

Before: [3, 1, 3, 1]
0 2 3 0
After:  [1, 1, 3, 1]

Before: [3, 2, 2, 2]
3 0 2 2
After:  [3, 2, 2, 2]

Before: [1, 3, 0, 3]
12 0 2 2
After:  [1, 3, 0, 3]

Before: [1, 3, 2, 1]
14 2 3 0
After:  [1, 3, 2, 1]

Before: [2, 2, 2, 2]
2 2 2 3
After:  [2, 2, 2, 4]

Before: [2, 1, 1, 2]
6 2 2 2
After:  [2, 1, 2, 2]

Before: [2, 0, 3, 3]
1 1 0 1
After:  [2, 0, 3, 3]

Before: [3, 2, 2, 3]
15 0 1 2
After:  [3, 2, 2, 3]



4 0 2 0
13 2 0 2
2 2 3 2
4 3 2 3
3 3 2 2
13 2 1 2
6 2 1 1
10 1 2 3
4 2 3 0
13 1 0 1
2 1 1 1
13 3 0 2
2 2 1 2
12 1 0 1
13 1 2 1
6 1 3 3
10 3 1 1
4 0 1 2
4 2 2 3
5 0 3 3
13 3 1 3
6 3 1 1
4 2 1 2
4 0 3 3
9 3 2 2
13 2 1 2
13 2 1 2
6 1 2 1
10 1 3 0
4 0 0 1
13 0 0 2
2 2 2 2
9 3 2 1
13 1 2 1
6 1 0 0
10 0 3 3
4 1 1 0
4 2 0 1
10 0 2 0
13 0 3 0
6 3 0 3
13 3 0 0
2 0 2 0
13 3 0 2
2 2 3 2
8 1 2 0
13 0 1 0
13 0 2 0
6 0 3 3
10 3 2 1
13 2 0 2
2 2 2 2
13 2 0 0
2 0 1 0
13 1 0 3
2 3 2 3
10 0 2 2
13 2 3 2
6 2 1 1
10 1 2 2
4 0 3 3
4 2 1 0
13 2 0 1
2 1 3 1
1 0 3 0
13 0 1 0
6 0 2 2
10 2 1 1
4 3 0 2
4 1 2 0
4 2 0 3
12 0 3 0
13 0 3 0
6 0 1 1
10 1 1 2
4 1 1 1
4 3 3 0
12 1 3 0
13 0 1 0
6 0 2 2
10 2 3 3
4 3 0 1
4 0 3 2
4 2 2 0
3 1 2 1
13 1 1 1
6 1 3 3
10 3 2 2
4 2 0 3
4 0 1 1
5 0 3 1
13 1 1 1
6 2 1 2
10 2 3 3
4 3 1 1
13 3 0 2
2 2 0 2
7 0 1 2
13 2 2 2
6 2 3 3
4 3 1 2
0 0 2 2
13 2 3 2
6 3 2 3
10 3 1 0
4 0 2 1
4 3 0 3
4 0 0 2
3 3 2 1
13 1 1 1
13 1 1 1
6 1 0 0
10 0 1 2
4 0 1 1
4 1 0 3
13 3 0 0
2 0 2 0
2 3 1 1
13 1 3 1
13 1 2 1
6 1 2 2
10 2 0 3
4 3 0 2
13 0 0 1
2 1 2 1
4 0 1 0
8 1 2 1
13 1 2 1
6 1 3 3
10 3 2 2
4 2 3 0
4 3 1 1
4 0 1 3
7 0 1 3
13 3 2 3
6 3 2 2
10 2 0 3
4 3 3 0
4 3 0 2
3 1 2 1
13 1 3 1
6 3 1 3
10 3 3 0
4 1 2 3
4 3 3 1
13 3 0 2
2 2 2 2
7 2 1 2
13 2 1 2
13 2 1 2
6 0 2 0
10 0 1 3
4 0 1 1
4 1 3 0
4 1 1 2
2 0 1 0
13 0 1 0
6 0 3 3
10 3 1 0
4 2 2 2
4 3 1 1
4 0 1 3
9 3 2 2
13 2 1 2
6 0 2 0
10 0 0 1
4 2 2 0
13 2 0 3
2 3 1 3
4 2 1 2
14 0 3 3
13 3 3 3
6 3 1 1
4 2 0 3
4 3 3 0
15 0 3 0
13 0 1 0
6 1 0 1
10 1 3 2
13 3 0 0
2 0 2 0
4 1 2 1
1 0 3 0
13 0 2 0
6 2 0 2
10 2 3 0
4 0 3 3
4 3 3 1
13 2 0 2
2 2 2 2
9 3 2 3
13 3 1 3
13 3 1 3
6 3 0 0
10 0 0 3
4 2 0 1
4 0 1 2
4 3 1 0
0 2 0 0
13 0 1 0
13 0 1 0
6 3 0 3
10 3 3 1
4 2 2 3
13 2 0 0
2 0 3 0
0 2 0 2
13 2 1 2
13 2 3 2
6 2 1 1
10 1 2 3
4 0 2 2
4 2 0 1
3 0 2 2
13 2 3 2
6 2 3 3
4 0 0 1
4 1 2 0
4 0 3 2
6 0 0 0
13 0 2 0
13 0 1 0
6 0 3 3
4 1 2 0
4 3 0 1
4 2 0 2
10 0 2 0
13 0 3 0
6 3 0 3
10 3 3 0
4 2 2 1
4 2 1 3
4 0 2 2
11 2 3 3
13 3 1 3
6 0 3 0
10 0 0 1
4 2 2 2
4 0 1 3
4 1 1 0
9 3 2 3
13 3 2 3
6 3 1 1
10 1 1 3
13 3 0 1
2 1 2 1
4 0 3 2
13 0 2 1
13 1 1 1
13 1 1 1
6 3 1 3
10 3 0 1
4 3 1 0
4 3 3 2
4 0 1 3
11 3 2 2
13 2 3 2
6 1 2 1
10 1 3 0
13 3 0 2
2 2 2 2
4 0 1 1
4 3 1 3
13 3 1 3
6 0 3 0
10 0 3 1
4 0 0 0
4 3 2 2
4 0 1 3
11 3 2 0
13 0 3 0
6 0 1 1
10 1 0 2
13 3 0 3
2 3 2 3
4 2 3 0
4 3 0 1
1 0 3 3
13 3 1 3
6 3 2 2
10 2 0 1
4 3 2 2
13 0 0 3
2 3 2 3
4 3 0 0
15 0 3 0
13 0 1 0
13 0 2 0
6 0 1 1
4 2 2 2
4 0 0 3
4 3 0 0
9 3 2 2
13 2 3 2
13 2 2 2
6 2 1 1
10 1 3 2
4 2 2 3
13 3 0 1
2 1 1 1
4 1 0 0
12 0 3 1
13 1 1 1
13 1 3 1
6 1 2 2
10 2 3 1
4 3 0 0
4 0 1 3
4 2 2 2
9 3 2 3
13 3 3 3
6 3 1 1
10 1 3 0
13 1 0 1
2 1 3 1
4 2 3 3
4 0 3 2
11 2 3 3
13 3 2 3
6 0 3 0
10 0 1 3
4 2 0 1
4 2 3 2
4 1 1 0
6 0 0 2
13 2 2 2
13 2 3 2
6 2 3 3
10 3 1 0
4 1 3 3
4 3 0 2
4 3 1 1
4 2 1 2
13 2 2 2
6 2 0 0
10 0 2 3
4 0 2 0
4 3 1 2
4 2 0 1
8 1 2 2
13 2 2 2
6 2 3 3
10 3 1 1
13 0 0 0
2 0 2 0
13 0 0 3
2 3 1 3
4 3 3 2
14 0 3 3
13 3 1 3
13 3 2 3
6 1 3 1
10 1 1 3
4 0 1 0
4 1 3 1
4 0 1 2
13 1 2 0
13 0 3 0
6 0 3 3
10 3 2 2
13 2 0 0
2 0 2 0
4 2 3 3
5 0 3 3
13 3 1 3
6 2 3 2
10 2 2 1
4 1 2 3
4 3 3 0
4 0 1 2
13 3 2 2
13 2 3 2
6 1 2 1
10 1 1 3
13 3 0 0
2 0 1 0
4 2 3 2
4 2 0 1
10 0 2 2
13 2 1 2
13 2 1 2
6 2 3 3
10 3 0 1
4 0 3 0
4 0 0 3
4 1 3 2
4 3 2 3
13 3 3 3
6 3 1 1
10 1 2 2
4 2 0 3
4 2 0 0
4 2 1 1
5 0 3 1
13 1 2 1
6 1 2 2
10 2 3 1
4 1 2 3
4 2 1 2
14 0 3 0
13 0 1 0
6 0 1 1
10 1 3 3
4 3 0 2
13 0 0 0
2 0 2 0
13 3 0 1
2 1 1 1
0 0 2 1
13 1 3 1
6 3 1 3
10 3 2 0
4 0 1 1
4 3 0 3
4 0 3 2
4 2 3 1
13 1 2 1
6 0 1 0
10 0 3 2
4 3 1 1
4 1 3 0
4 2 1 3
12 0 3 1
13 1 1 1
6 2 1 2
4 3 2 1
4 2 0 0
4 0 0 3
1 0 3 1
13 1 3 1
6 2 1 2
10 2 1 1
4 3 3 0
4 0 0 2
0 2 0 3
13 3 2 3
6 3 1 1
10 1 0 0
4 0 0 3
4 2 0 1
13 0 0 2
2 2 3 2
8 1 2 3
13 3 3 3
6 0 3 0
10 0 0 2
4 1 1 1
4 2 3 0
4 2 0 3
5 0 3 1
13 1 1 1
13 1 3 1
6 2 1 2
10 2 1 0
4 1 1 1
4 0 1 3
4 2 0 2
9 3 2 3
13 3 1 3
6 0 3 0
10 0 3 1
13 2 0 0
2 0 2 0
4 3 1 3
15 3 0 0
13 0 3 0
13 0 1 0
6 1 0 1
10 1 1 0
13 1 0 1
2 1 1 1
13 3 0 3
2 3 2 3
1 2 3 3
13 3 2 3
6 0 3 0
10 0 1 2
4 2 1 0
4 0 2 1
4 2 1 3
5 0 3 0
13 0 3 0
6 0 2 2
10 2 0 3
13 0 0 0
2 0 2 0
4 3 2 2
4 1 3 1
13 1 2 2
13 2 2 2
6 2 3 3
10 3 0 1
13 0 0 3
2 3 3 3
4 3 1 0
4 0 2 2
0 2 0 3
13 3 1 3
6 1 3 1
10 1 0 0
4 3 2 2
4 0 3 3
4 2 0 1
11 3 2 1
13 1 3 1
6 0 1 0
4 1 3 1
4 1 1 3
13 3 2 3
13 3 3 3
13 3 2 3
6 0 3 0
10 0 2 1
4 0 0 3
4 0 2 0
11 3 2 2
13 2 1 2
6 2 1 1
4 2 2 0
4 3 1 2
4 2 2 3
5 0 3 3
13 3 2 3
13 3 3 3
6 3 1 1
10 1 2 0
4 0 2 3
13 0 0 1
2 1 2 1
8 1 2 1
13 1 3 1
6 0 1 0
4 0 0 1
4 2 3 2
1 2 3 1
13 1 1 1
13 1 3 1
6 1 0 0
10 0 2 1
13 1 0 0
2 0 0 0
4 0 1 2
13 0 0 3
2 3 2 3
11 2 3 2
13 2 2 2
13 2 1 2
6 2 1 1
13 1 0 0
2 0 2 0
4 0 3 3
4 2 2 2
1 2 3 0
13 0 3 0
6 0 1 1
10 1 3 0
4 2 0 1
4 3 1 2
1 1 3 2
13 2 1 2
6 2 0 0
10 0 3 2
4 2 2 0
4 1 0 1
13 3 0 3
2 3 2 3
12 1 0 0
13 0 1 0
6 2 0 2
4 2 1 0
5 0 3 0
13 0 2 0
6 0 2 2
10 2 3 1
13 2 0 0
2 0 3 0
4 0 2 3
4 2 1 2
8 2 0 3
13 3 1 3
6 1 3 1
10 1 1 2
4 2 2 0
13 2 0 1
2 1 3 1
4 2 3 3
7 0 1 0
13 0 2 0
6 2 0 2
10 2 2 1
4 0 1 3
4 0 2 0
4 2 3 2
9 3 2 3
13 3 2 3
6 1 3 1
4 1 0 0
4 3 2 3
10 0 2 2
13 2 1 2
6 1 2 1
10 1 0 2
4 3 1 1
4 2 2 0
4 0 1 3
7 0 1 0
13 0 2 0
13 0 3 0
6 2 0 2
10 2 3 3
4 2 2 0
13 3 0 1
2 1 1 1
4 0 0 2
13 1 2 2
13 2 2 2
13 2 1 2
6 2 3 3
4 0 3 2
4 3 2 0
0 2 0 1
13 1 1 1
6 3 1 3
10 3 2 1
13 0 0 2
2 2 3 2
4 1 2 3
4 2 1 0
0 0 2 2
13 2 1 2
13 2 2 2
6 2 1 1
10 1 2 2
4 3 0 1
13 2 0 3
2 3 2 3
7 0 1 1
13 1 1 1
13 1 1 1
6 2 1 2
10 2 3 3
4 3 3 1
4 1 1 0
4 2 0 2
7 2 1 2
13 2 3 2
6 3 2 3
4 2 2 1
4 3 1 0
4 0 0 2
3 0 2 1
13 1 3 1
6 1 3 3
10 3 1 0
4 2 0 1
4 2 3 2
13 3 0 3
2 3 0 3
1 1 3 1
13 1 2 1
6 0 1 0
10 0 0 1
4 1 1 2
13 3 0 3
2 3 3 3
4 2 3 0
15 3 0 0
13 0 1 0
13 0 3 0
6 0 1 1
4 1 0 0
4 1 2 3
13 0 0 2
2 2 2 2
10 0 2 3
13 3 3 3
13 3 3 3
6 3 1 1
4 3 0 0
13 1 0 3
2 3 2 3
7 2 0 0
13 0 2 0
13 0 3 0
6 0 1 1
13 2 0 0
2 0 0 0
13 2 0 3
2 3 1 3
4 3 0 2
13 3 2 0
13 0 3 0
6 1 0 1
10 1 1 0
4 0 0 2
4 1 3 1
13 1 2 2
13 2 2 2
6 2 0 0
4 1 1 2
4 3 0 1
4 2 0 3
15 1 3 1
13 1 1 1
13 1 3 1
6 1 0 0
13 2 0 2
2 2 2 2
4 0 3 3
4 0 3 1
9 3 2 2
13 2 3 2
6 2 0 0
4 2 0 2
4 2 0 1
9 3 2 3
13 3 3 3
13 3 3 3
6 3 0 0
10 0 0 1
4 2 1 0
4 1 3 2
4 2 1 3
5 0 3 2
13 2 2 2
13 2 1 2
6 2 1 1
4 0 0 0
13 0 0 2
2 2 0 2
13 1 0 3
2 3 1 3
6 3 3 0
13 0 2 0
13 0 1 0
6 0 1 1
4 1 2 0
4 2 1 3
6 0 0 2
13 2 1 2
13 2 1 2
6 2 1 1
10 1 1 2
4 1 2 3
4 0 2 1
4 2 0 0
14 0 3 1
13 1 2 1
13 1 2 1
6 2 1 2
10 2 2 0
4 1 2 2
4 3 2 3
4 3 0 1
3 1 2 2
13 2 1 2
13 2 2 2
6 2 0 0
10 0 1 3
4 3 3 0
4 2 1 2
4 2 3 1
7 2 0 2
13 2 3 2
6 3 2 3
10 3 3 1
4 2 2 0
4 2 3 3
4 3 2 2
5 0 3 2
13 2 1 2
13 2 2 2
6 1 2 1
4 1 2 0
13 3 0 3
2 3 0 3
4 3 2 2
11 3 2 0
13 0 3 0
13 0 2 0
6 0 1 1
4 0 0 2
4 1 1 0
4 3 2 3
3 3 2 3
13 3 2 3
6 1 3 1
4 2 3 3
4 2 0 0
11 2 3 0
13 0 2 0
6 0 1 1
10 1 3 3
4 1 1 0
13 3 0 1
2 1 0 1
2 0 1 0
13 0 3 0
6 0 3 3
10 3 2 2
13 1 0 0
2 0 2 0
4 3 3 1
4 1 0 3
14 0 3 0
13 0 2 0
6 2 0 2
4 1 3 0
4 2 0 3
4 2 2 1
12 0 3 3
13 3 3 3
6 2 3 2
10 2 2 1
4 3 3 2
13 0 0 3
2 3 1 3
6 3 3 0
13 0 1 0
6 0 1 1
10 1 0 3
4 1 0 0
4 0 2 1
4 2 1 2
10 0 2 1
13 1 2 1
6 3 1 3
10 3 3 0
4 0 2 1
4 3 3 2
4 0 1 3
11 3 2 3
13 3 3 3
13 3 2 3
6 3 0 0
4 0 0 2
4 3 3 1
13 1 0 3
2 3 2 3
11 2 3 2
13 2 3 2
6 0 2 0
10 0 3 3
4 0 0 2
4 1 0 1
4 1 0 0
13 1 2 2
13 2 2 2
6 3 2 3
10 3 2 2
4 0 0 1
4 0 2 3
2 0 1 1
13 1 3 1
13 1 1 1
6 2 1 2
10 2 3 0
4 3 0 1
4 1 1 3
4 0 3 2
2 3 1 3
13 3 2 3
13 3 1 3
6 0 3 0
10 0 1 1
4 2 3 2
4 0 3 3
4 1 1 0
9 3 2 2
13 2 3 2
6 1 2 1
4 2 3 3
4 2 2 0
13 2 0 2
2 2 1 2
5 0 3 0
13 0 3 0
6 0 1 1
4 0 2 2
4 3 0 0
11 2 3 3
13 3 2 3
13 3 3 3
6 1 3 1
10 1 0 0
4 0 3 3
13 1 0 1
2 1 3 1
4 2 3 2
7 2 1 3
13 3 2 3
6 0 3 0
"