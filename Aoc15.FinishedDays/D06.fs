module Aoc15.D06

open Aoc15
open Input
open TextCopy

let input =  "input.txt" |> f2text

type Switch = ON | OFF | TOGGLE

let handle (sw:Switch, pos1, pos2) (config: bool[,]) =
    for x in (fst pos1) .. (fst pos2) do
        for y in (snd pos1) .. (snd pos2) do
            match sw with 
            | ON -> config[x,y] <- true
            | OFF -> config[x,y] <- false
            | TOGGLE -> config[x,y] <- not config[x,y]
    config

let handle2 (sw:Switch, pos1, pos2) (config: int[,]) =
    for x in (fst pos1) .. (fst pos2) do
        for y in (snd pos1) .. (snd pos2) do
            match sw with 
            | ON -> config[x,y] <- config[x,y]+1
            | OFF -> config[x,y] <- max 0 (config[x,y]-1)
            | TOGGLE -> config[x,y] <- config[x,y]+2
    config

let countTrue (config: bool[,]) =
    let nums = 
        seq { 
            for x in 0 .. Array2D.length1 config-1 do
                for y in 0 .. Array2D.length2 config-1 do
                    if config[x,y] then 1 else 0
        }
        
    nums |> Seq.sum

let sumElts (config: int[,]) =
    let nums = 
        seq { 
            for x in 0 .. Array2D.length1 config-1 do
                for y in 0 .. Array2D.length2 config-1 do
                    config[x,y]
        }
        
    nums |> Seq.sum


let parseLine (ln:string) =
    ln.Replace("toggle", "_ toggle")
    |> text2tokens " ,"
    |> fun strs -> ((strs[1] |> function | "on" -> ON | "off" -> OFF | "toggle" -> TOGGLE), (int strs[2], int strs[3]), (int strs[5], int strs[6]))
    //|> List.map int
    //|> fun (x::y::z::[]) -> (x,y,z)

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine

let solve1 (text:string) = 
    let instructions = 
        text
        |> parse2lines
        |> List.map (fun tpl -> handle tpl)
    
    let arr0 = Array2D.init 1000 1000 (fun x y -> false)
    let arrN = instructions |> List.fold (fun st ins -> ins st) arr0
    countTrue arrN
    
    
let solve2 (text:string) =
    let instructions = 
        text
        |> parse2lines
        |> List.map (fun tpl -> handle2 tpl)
    
    let arr0 = Array2D.init 1000 1000 (fun x y -> 0)
    let arrN = instructions |> List.fold (fun st ins -> ins st) arr0
    sumElts arrN

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()