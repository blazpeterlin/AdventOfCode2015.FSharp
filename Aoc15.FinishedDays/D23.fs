module Aoc15.D23

open Aoc15
open Input
open TextCopy
open System.Collections.Generic
open Aoc15.Operators

let input =  "input.txt" |> f2text

type OP = HLF of char | TPL of char | INC of char | JMP of int64 | JIE of char*int64 | JIO of char*int64

let parseLine (ln:string) =
    ln
    |> text2tokens ", "
    |> fun xs -> 
        match xs[0] with
        | "hlf" -> HLF(xs[1][0])
        | "tpl" -> TPL(xs[1][0])
        | "inc" -> INC(xs[1][0])
        | "jmp" -> JMP(int64 xs[1])
        | "jie" -> JIE(xs[1][0], int64 xs[2])
        | "jio" -> JIO(xs[1][0], int64 xs[2])
            
    // |> text2tokensStr ["abc";"def"]

let doOp (rs:Map<char,int64>) (ins:int) (op:OP) =
    match op with
    | HLF(r) -> (rs |> Map.add r (rs[r]/2L)),ins+1
    | TPL(r) -> (rs |> Map.add r (rs[r]*3L)),ins+1
    | INC(r) -> (rs |> Map.add r (rs[r]+1L)),ins+1
    | JMP(jmp) -> rs,ins+(int jmp)
    | JIE(r,offset) -> rs, if rs[r]%2L=0L then ins+(int offset) else ins+1
    | JIO(r,offset) -> rs, if rs[r]=1L then ins+(int offset) else ins+1

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine

let solve1 (text:string) = 
    let program = text |> parse2lines
    let r0 = ['a',0L;'b',0L] |> Map.ofList

    let st0 = r0,0

    let stN =
        st0
        |> Seq.unfold (fun (rs, ins) -> 
            if ins<0 || ins>=program.Length then None else
            let next = doOp rs ins program[ins]
            Some (next,next)
        )
        |> Seq.last

    let rN = fst stN
    rN['b']
    
let solve2 (text:string) =
    let program = text |> parse2lines
    let r0 = ['a',1L;'b',0L] |> Map.ofList

    let st0 = r0,0

    let stN =
        st0
        |> Seq.unfold (fun (rs, ins) -> 
            if ins<0 || ins>=program.Length then None else
            let next = doOp rs ins program[ins]
            Some (next,next)
        )
        |> Seq.last

    let rN = fst stN
    rN['b']

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()