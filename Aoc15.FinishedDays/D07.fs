module Aoc15.D07

open Aoc15
open Input
open TextCopy
open System.Collections.Generic

let input =  "input.txt" |> f2text

type OP = 
    ASSIGN of string*string
    | AND of string*string*string 
    | OR of string*string*string 
    | LSHIFT  of string*string*string 
    | RSHIFT  of string*string*string 
    | NOT  of string*string 

let parseArg (arg:string) (registers: Dictionary<string,uint16>) =
    match System.UInt16.TryParse arg with
    | true,num -> Some num
    | false,_ -> if registers.ContainsKey arg then Some registers[arg] else None

let handle OP (registers: Dictionary<string,uint16>) =
    let regNext = Dictionary<string,uint16>(registers)
    match OP with
    | ASSIGN(reg1, sink) -> 
        if registers.ContainsKey sink then Some registers else
        let arg1 = parseArg reg1 registers
        if arg1 = None then None else
            registers[sink] <- uint16 arg1.Value
            Some registers
    | AND(reg1,reg2,sink) -> 
        let arg1, arg2 = parseArg reg1 registers, parseArg reg2 registers
        if arg1 = None or arg2 = None then None else
            registers[sink] <- arg1.Value &&& arg2.Value
            Some registers
    | OR(reg1,reg2,sink) -> 
        let arg1, arg2 = parseArg reg1 registers, parseArg reg2 registers
        if arg1 = None or arg2 = None then None else
            registers[sink] <- arg1.Value ||| arg2.Value
            Some registers
    | LSHIFT(reg1,reg2,sink) -> 
        let arg1, arg2 = parseArg reg1 registers, parseArg reg2 registers
        if arg1 = None or arg2 = None then None else
            registers[sink] <- arg1.Value <<< (int arg2.Value)
            Some registers
    | RSHIFT(reg1,reg2,sink) -> 
        let arg1, arg2 = parseArg reg1 registers, parseArg reg2 registers
        if arg1 = None or arg2 = None then None else
            registers[sink] <- arg1.Value >>> (int arg2.Value)
            Some registers
    | NOT(reg1,sink) -> 
        let arg1 = parseArg reg1 registers
        if arg1 = None then None else
            registers[sink] <- ~~~(arg1.Value)
            Some registers

let parseLine (ln:string) =
    ln
    |> text2tokens " "
    |> fun tkns ->
        if tkns[0]="NOT" then NOT(tkns[1], tkns[3]) else
        match tkns[1] with
        | "->" -> ASSIGN(tkns[0], tkns[2])
        | "AND" -> AND(tkns[0], tkns[2], tkns[4])
        | "OR" -> OR(tkns[0], tkns[2], tkns[4])
        | "LSHIFT" -> LSHIFT(tkns[0], tkns[2], tkns[4])
        | "RSHIFT" -> RSHIFT(tkns[0], tkns[2], tkns[4])
        | _ -> failwith "huh"
    //|> List.map int
    //|> fun (x::y::z::[]) -> (x,y,z)

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine

let solve1 (text:string) = 
    let handles = 
        text
        |> parse2lines
        |> List.map (fun op -> handle op)
    let reg0 = Dictionary<string,uint16>()
    let iterHandles rStart = 
        handles |> List.fold (fun st h -> match h st with | Some dict -> dict | None -> st ) rStart

    let regN = reg0 |> Seq.unfold (fun r -> match iterHandles r with | res -> Some(res,res)) |> Seq.find (fun r -> r.ContainsKey "a")
    regN["a"]
    
let solve2 (text:string) =
    let handles = 
        text
        |> parse2lines
        |> List.map (fun op -> handle op)
    let reg0 = Dictionary<string,uint16>()
    reg0["b"] <- solve1 text
    let iterHandles rStart = 
        handles |> List.fold (fun st h -> match h st with | Some dict -> dict | None -> st ) rStart

    let regN = reg0 |> Seq.unfold (fun r -> match iterHandles r with | res -> Some(res,res)) |> Seq.find (fun r -> r.ContainsKey "a")
    regN["a"]

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()