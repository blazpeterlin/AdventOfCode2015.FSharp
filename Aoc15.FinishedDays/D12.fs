module Aoc15.D12

open Aoc15
open Input
open TextCopy
open System.Collections.Generic
open Aoc15.Operators

let input =  "input.txt" |> f2text

let sumNums (str:string) =
    str 
    |> text2tokens "abcdefghijklmnoprstuvzxqyw\",:[]{}"
    |> filter (fun x -> x<>"")
    |> map int
    |> sum

let findLevel0 (dir:int) (str:string) =
    let start = if dir=1 then 0 else str.Length-1
    let fin = str.Length - 1 - start

    [start .. dir .. fin]
    |> List.scan (fun (lvl,_) idx ->
            let lvlNext = match str[idx] with 
                | '{' -> lvl + (if dir=1 then 1 else -1)
                | '}' -> lvl + (if dir=1 then -1 else 1)
                | _ -> lvl
            (lvlNext, idx)
        ) (1,-1)
    |> List.find (fun (lvl, idx) -> lvl=0)
    |> snd


let removeRed (str:string) = 
    let idx = str.IndexOf ":\"red\""
    if idx = -1 then str else
    let left = findLevel0 (-1) (str.Substring(0, idx))
    let right = findLevel0 (+1) (str.Substring(idx))
    str.Substring(0, left) + str.Substring(idx+1+right)

let parseLine (ln:string) =
    ln
    //|> text2tokens "x"
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine
    |> head

let solve1 (text:string) = 
    text
    |> parse2lines
    |> sumNums
    
let solve2 (text:string) =
    let ln0 = text |> parse2lines
    
    let lnGreen =  
        ln0
        |> Seq.unfold (fun str ->
                let grn = removeRed str
                if grn = str then None
                else Some (grn, grn)
            )
        |> Seq.last

    lnGreen |> sumNums

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()