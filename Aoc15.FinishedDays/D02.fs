module Aoc15.D02

open Aoc15
open Input
open TextCopy

let input =  "input.txt" |> f2text

let surface (x,y,z) =
    2 * (x*y + y*z + z*x) + (x*y*z/(List.max [x;y;z]))

let ribbon (x,y,z) =
    let smaller = [x;y;z] |> List.sortDescending |> List.skip 1
    let regular = smaller |> List.map (fun x -> 2 * x) |> List.sum
    let bow = x*y*z
    regular+bow

let parseLine ln =
    text2tokens "x" ln
    |> List.map int
    |> fun (x::y::z::[]) -> (x,y,z)

let parseAll strs = strs |> text2lines |> List.map parseLine

let solve1 (text:string) = 
    let all = text |> parseAll
    all |> List.map surface |> List.sum
    
let solve2 (text:string) =
    let all = text |> parseAll
    all |> List.map ribbon |> List.sum

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()