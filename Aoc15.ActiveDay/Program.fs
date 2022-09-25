module Aoc15.ActiveDay

open Aoc15
open Input
open TextCopy
open System.Collections.Generic
open Operators

let input =  "input.txt" |> f2text

let parseLine (ln:string) =
    ln
    |> text2tokens "x"
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine

let solve1 (text:string) = 
    0
    
let solve2 (text:string) =
    0

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res1.ToString())

let finished = true
()