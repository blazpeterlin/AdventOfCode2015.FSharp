module Aoc15.D03

open Aoc15
open Input
open Operators
open TextCopy

let input =  "input.txt" |> f2text

let parseLine (ln:string) =
    ln.ToCharArray()
    |> Array.toList
    //|> List.map int
    //|> fun (x::y::z::[]) -> (x,y,z)

let parse2lines strs = 
    strs 
    |> text2lines 
    |> List.map parseLine

let steps2locs steps = 
    steps
    |> List.map (function | '^' -> (0,1) | 'v' -> (0,-1) | '<' -> (-1,0) | '>' -> (1, 0) | ch -> failwith $"bad char {ch}")
    |> List.scan (fun (st:int*int) delta -> st +.. delta) (0,0)

let solve1 (text:string) = 
    text
    |> text2lines
    |> List.head
    |> parseLine
    |> steps2locs
    |> List.distinct
    |> List.length
    
let solve2 (text:string) =
    let resSplit spl =
        text
        |> text2lines
        |> List.head
        |> parseLine
        |> spl
        |> steps2locs

    let locs1 = resSplit (fun lst -> lst |> List.indexed |> List.filter(fun (i,_) -> i%2=0) |> List.map snd)
    let locs2 = resSplit (fun lst -> lst |> List.indexed |> List.filter(fun (i,_) -> i%2=1) |> List.map snd)

    List.append locs1 locs2
    |> List.distinct
    |> List.length

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()