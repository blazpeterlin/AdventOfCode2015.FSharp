module Aoc15.ActiveDay

open Aoc15
open Input
open TextCopy

let input =  "input.txt" |> f2text

let parseLine ln =
    ln
    |> text2tokens "x"
    |> List.map int
    |> fun (x::y::z::[]) -> (x,y,z)

let parse2lines strs = 
    strs 
    |> text2lines 
    |> List.map parseLine

let solve1 (text:string) = 
    0
    
let solve2 (text:string) =
    0

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()