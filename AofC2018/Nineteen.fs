module Nineteen

type Registers = {a:int;b:int;c:int;d:int;e:int;f:int} 
    with static member Default = {a=0;b=0;c=0;d=0;e=0;f=0}

type Values = {a:int;b:int;c:int}

type OpFun = Values -> Registers -> Registers

type OpKey = |Addr|Addi|Mulr|Muli|Banr|Bani|Borr|Bori|Setr|Seti|Gtir|Gtri|Gtrr|Eqir|Eqri|Eqrr

type ItrOp = {op:OpKey;v:Values}

type ItrPtr = {ptr:int}

type Instruction = | ItrOp of ItrOp | ItrPtr of ItrPtr

let private reg (r:Registers) (i:int) =
    match i with 
    | 0 -> r.a 
    | 1 -> r.b 
    | 2 -> r.c 
    | 3 -> r.d 
    | 4 -> r.e
    | 5 -> r.f
    | _ -> failwith "invalid register" 

let private setReg (r:Registers) (i:int) (value:int) =
    match i with 
    | 0 -> {r with a = value}
    | 1 -> {r with b = value}
    | 2 -> {r with c = value} 
    | 3 -> {r with d = value} 
    | 4 -> {r with e = value} 
    | 5 -> {r with f = value} 
    | _ -> failwith "invalid register" 

let toOpFun (key:OpKey) =
    match key with
    | Addr -> fun v r -> setReg r v.c (reg r v.a + reg r v.b)
    | Addi -> fun v r -> setReg r v.c (reg r v.a + v.b)
    | Mulr -> fun v r -> setReg r v.c (reg r v.a * reg r v.b)
    | Muli -> fun v r -> setReg r v.c (reg r v.a * v.b)
    | Banr -> fun v r -> setReg r v.c (reg r v.a &&& reg r v.b)
    | Bani -> fun v r -> setReg r v.c (reg r v.a &&& v.b)
    | Borr -> fun v r -> setReg r v.c (reg r v.a ||| reg r v.b)
    | Bori -> fun v r -> setReg r v.c (reg r v.a ||| v.b)
    | Setr -> fun v r -> setReg r v.c (reg r v.a)
    | Seti -> fun v r -> setReg r v.c (v.a)
    | Gtir -> fun v r -> setReg r v.c (if v.a > reg r v.b then 1 else 0)
    | Gtri -> fun v r -> setReg r v.c (if reg r v.a > v.b then 1 else 0)
    | Gtrr -> fun v r -> setReg r v.c (if reg r v.a > reg r v.b then 1 else 0)
    | Eqir -> fun v r -> setReg r v.c (if v.a = reg r v.b then 1 else 0)
    | Eqri -> fun v r -> setReg r v.c (if reg r v.a = v.b then 1 else 0)
    | Eqrr -> fun v r -> setReg r v.c (if reg r v.a = reg r v.b then 1 else 0)

let private toOpKey (s:string) =
    match s with
    | "addr" -> Addr
    | "addi" -> Addi
    | "mulr" -> Mulr
    | "muli" -> Muli
    | "banr" -> Banr
    | "bani" -> Bani
    | "borr" -> Borr
    | "bori" -> Bori
    | "setr" -> Setr
    | "seti" -> Seti
    | "gtir" -> Gtir
    | "gtri" -> Gtri
    | "gtrr" -> Gtrr
    | "eqir" -> Eqir
    | "eqri" -> Eqri
    | "eqrr" -> Eqrr
    | _      -> failwith "unknown operation key"

let private compute (ptr:int) (program:ItrOp array) (init:Registers) =
    let rec loop (i:int) (r:Registers) =  
        if i >= program.Length then r else
        let io = program.[i]    
        let r' = setReg r ptr i 
        let r'' = toOpFun io.op io.v r'
        let i' = reg r'' ptr + 1        
        loop i' r''
    loop 0 init

let private compute2 (ptr:int) (program:ItrOp array) (init:Registers) (initI:int) =
    let rec loop (i:int) (r:Registers) =  
        if i >= program.Length then r else
        let io = program.[i]    
        let r' = setReg r ptr i 
        let r'' = toOpFun io.op io.v r'
        let i' = reg r'' ptr + 1
        loop i' r''
    loop initI init

let private parseLine (l:string) : Instruction =
    match l with 
    | Parsing.Regex @"#ip (\d+)" [ptr] -> 
        ItrPtr {ptr=int ptr}
    | Parsing.Regex @"([a-z]{4})\s(\d+)\s(\d+)\s(\d+)" [key;a;b;c] -> 
        ItrOp {op=(toOpKey key);v={a=int a;b=int b;c=int c}}
    | _ -> failwith "unable to parse instruction line"

let execute = fun d-> 
    let instr = d |> Parsing.splitLines |> Seq.map (parseLine) |> List.ofSeq
    let bound = instr |> Seq.choose (fun i -> match i with | ItrPtr ip -> Some ip | _ -> None) |> Seq.head
    let program = instr |> Seq.choose (fun i -> match i with | ItrOp op -> Some op | _ -> None) |> Array.ofSeq
    let result1 = compute bound.ptr program Registers.Default
    printfn "Day 19 - Part 1: Value in register 0 is %i" result1.a
    // Upon inspection of the program execution, there are two nested loops counting up until each is greater than 10551417 (reg F)
    // Within these loops we are also looking for the factors of 10551417 and setting reg A to the factor of the outer loop
    // In this case the only factors are 3 & 3517139
    // Once the outer loop condition is met, reg F is added to reg A and set to reg A
    // Setting these states just before the final logic is executed is done below
    let r = {a=3517143;b=10551417;c=8;d=0;e=10551416;f=10551417}
    let result2 = compute2 2 program r 13
    printfn "Day 19 - Part 1: Value in register 0 is %i" result2.a

let testSet = @"
#ip 0
seti 5 0 1
seti 6 0 2
addi 0 1 0
addr 1 2 3
setr 1 0 0
seti 8 0 4
seti 9 0 5"

let dataSet = @"
#ip 2
addi 2 16 2
seti 1 0 4
seti 1 0 1
mulr 4 1 3
eqrr 3 5 3
addr 3 2 2
addi 2 1 2
addr 4 0 0
addi 1 1 1
gtrr 1 5 3
addr 2 3 2
seti 2 5 2
addi 4 1 4
gtrr 4 5 3
addr 3 2 2
seti 1 9 2
mulr 2 2 2
addi 5 2 5
mulr 5 5 5
mulr 2 5 5
muli 5 11 5
addi 3 8 3
mulr 3 2 3
addi 3 5 3
addr 5 3 5
addr 2 0 2
seti 0 9 2
setr 2 8 3
mulr 3 2 3
addr 2 3 3
mulr 2 3 3
muli 3 14 3
mulr 3 2 3
addr 5 3 5
seti 0 6 0
seti 0 0 2
"
