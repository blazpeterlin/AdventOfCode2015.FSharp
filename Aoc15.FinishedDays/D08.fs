module Aoc15.D08

open Aoc15
open Input
open TextCopy

let input =  "input.txt" |> f2text

let numCode (str: string) = str.Length

let numMem (str: string) =
    str.ToCharArray() 
    |> Seq.fold 
        (fun (count,wasSlash,slashExtra) char -> 
            if wasSlash && slashExtra>0 then (count,slashExtra>1,slashExtra-1)
            else if wasSlash && char='x' then (count,true,2)
            else if wasSlash then (count,false,0)
            else if char='\\' then (count+1,true,0)
            else (count+1,false,0)
        )
        (0,false,0)
    |> fun (count,_,_) -> count-2

let diff1 (str:string) = (numCode str) - (numMem str)

let encode (str:string) =
    str.ToCharArray()
    |> Seq.map (fun ch -> if ch = '\\' then "\\\\" else if ch = '"' then "\\\"" else ch.ToString())
    |> fun strs -> "\"" + (strs |> String.concat "") + "\""

let diff2 (str:string) = (str |> encode |> numCode) - (numCode str)

let parseLine (ln:string) =
    ln
    //|> text2tokens "x"
    //|> List.map int
    //|> fun (x::y::z::[]) -> (x,y,z)

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine

let solve1 (text:string) = 
    text
    |> parse2lines
    |> List.map diff1
    |> List.sum
    
let solve2 (text:string) =
    text
    |> parse2lines
    |> List.map diff2
    |> List.sum

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()