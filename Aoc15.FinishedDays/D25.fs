module Aoc15.D25

open Aoc15
open Input
open TextCopy
open System.Collections.Generic
open Aoc15.Operators

let input =  "input.txt" |> f2text

let parseLine (ln:string) =
    ln
    |> text2tokensStr ["To continue, please consult the code grid in the manual.  Enter the code at row ";", column ";"."]
    |> fun xs -> int xs[0], int xs[1]

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine
    |> head

let nextCode code =
    (code * 252533L) % 33554393L

let solve1 (text:string) = 
    let row,column = text |> parse2lines

    let code0 = 20151125L

    let desiredIdx = 
        (row,column)
        |> Seq.unfold (fun (r,c) ->
            if r=1 && c=1 then None
            else if c>1 then 
                let r = (r+1,c-1) 
                Some(r,r)
            else 
                let r = (1,r-1)
                Some (r,r)
        )
        |> Seq.length

    let res = 
        code0
        |> Seq.unfold (fun code -> 
            let nc = nextCode code
            Some(code,nc))
        |> Seq.item desiredIdx

    res

let res1 = input |> solve1

ClipboardService.SetText(res1.ToString())

let finished = true
()