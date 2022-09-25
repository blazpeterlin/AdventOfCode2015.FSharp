module Aoc15.D10

open Aoc15
open Input
open TextCopy
open System.Collections.Generic
open Aoc15.Operators

let input =  "input.txt" |> f2text

let lookAndSay (str:int list) =
    let acc0 = (str[0],0,[])
    //for ch in (str.ToCharArray() |> List.ofArray) do
    let res = 
        (str @ [-1])
        |> List.fold (fun (lastCh, count,str) ch ->
                if ch=lastCh then lastCh,count+1,str
                else 
                    let countAsStr = 
                        if count < 10 
                        then [count] 
                        else count.ToString().ToCharArray()|>ofArray|>List.map int
                    let rev = countAsStr |> List.rev
                    //let strNext = str@ countAsStr @[lastCh]
                    let strNext = lastCh::(rev@str)
                    ch,1,strNext
            )
            acc0
        |> fun (_,_,str) -> str
    res |> List.rev
        

let parseLine (ln:string) =
    ln.ToCharArray() |> ofArray |> map (fun ch -> ch.ToString() |> int)
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine
    |> List.head

let solve1 (text:string) = 
    let ln = text |> parse2lines
    let res = [1..40] |> Seq.fold (fun st i -> lookAndSay st) ln
    res.Length
    
let solve2 (text:string) =
    let ln = text |> parse2lines
    let res = [1..50] |> Seq.fold (fun st i -> lookAndSay st) ln
    res.Length

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()