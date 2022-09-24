module Aoc15.D01

open Aoc15
open Input
open TextCopy

let input =  "input.txt" |> f2text

let solve1 (text:string) = 
    text.ToCharArray() 
    |> Array.toList 
    |> List.map( fun ch -> if ch = '(' then 1 else -1 )
    |> List.sum
let solve2 (text:string) =
    text.ToCharArray() 
        |> Array.toList 
        |> List.map( fun ch -> if ch = '(' then 1 else -1 )
        |> List.scan (fun (sum,step) num -> (sum+num,step+1)) (0,0)
        |> List.find (fun (sum,step) -> sum= -1)
        |> snd

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res1.ToString())

//Clipboard
let finished = true
()