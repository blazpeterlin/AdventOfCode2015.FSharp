module Aoc15.D14

open Aoc15
open Input
open TextCopy
open System.Collections.Generic
open Aoc15.Operators

let input =  "input.txt" |> f2text

type State = { Name:string; FlySpeed:int; FlyDur:int; RestDur:int; }
type Mode = FLY | REST

let parseLine (ln:string) =
    ln
    //|> text2tokens "x"
    |> text2tokensStr [" can fly ";" km/s for ";" seconds, but then must rest for ";" seconds."]
    |> fun xs -> { Name=xs[0]; FlySpeed= int xs[1]; FlyDur= int xs[2];RestDur= int xs[3]}

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine

let unfoldStateInf (st:State) = 
    (0, FLY, st.FlyDur)
        |> Seq.unfold (fun (len, mode, untilNextMode) ->
            let r = 
                match mode with
                | FLY -> (len+st.FlySpeed, (if untilNextMode=1 then REST else FLY), if untilNextMode=1 then st.RestDur else untilNextMode-1)
                | REST -> (len, (if untilNextMode=1 then FLY else REST), if untilNextMode=1 then st.FlyDur else untilNextMode-1)
            Some (r,r)
        )

let solve1 (text:string) = 
    let totalSecs = 2503
    let sts = text |> parse2lines
    let expand st =
        st
        |> unfoldStateInf
        |> Seq.item (totalSecs-1)
    let res = sts |> map expand |> map (fun (len,_,_) -> len) |> List.max
    res
        
    
    
let solve2 (text:string) =
    let totalSecs = 2503
    let sts = text |> parse2lines
    let expand st =
        st
        |> unfoldStateInf
        |> Seq.take (totalSecs-1)
        |> Seq.map (fun (len,_,_) -> len)
        |> Seq.indexed
        |> Seq.map (fun (idx,len) -> idx, (len, st.Name))
    let res = 
        sts 
        |> map expand |> Seq.concat |> List.ofSeq
        |> List.groupBy fst 
        |> map (fun grp -> grp |> snd |> List.maxBy (snd >> fst) |> snd |> snd)
        |> List.countBy id
        |> List.maxBy snd
        |> snd
    res

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()