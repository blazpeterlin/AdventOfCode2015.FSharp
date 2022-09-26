module Aoc15.D24

open Aoc15
open Input
open TextCopy
open System.Collections.Generic
open Aoc15.Operators

let input =  "input.txt" |> f2text

let parseLine (ln:string) =
    ln |>int
    // |> text2tokens "x"
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine

let quantumEntanglement (elts: int list) =
    elts |> List.map int64 |> List.reduce (*)

let fillUntil minGroupSize (halve:bool) (quota:int) (elts:int list) =
    let pq = PriorityQueue()
    //let grp0 = [elts[0]],elts.Tail
    //pq.Enqueue (grp0, elts[0])
    let grp0 = if halve then [elts[0]],elts.Tail else [],elts
    pq.Enqueue (grp0, (grp0|>fst|>List.sum))

    seq {
        while pq.Count>0 do
            let built,remaining = pq.Dequeue()
            let totalSoFar = built |> List.sum

            if remaining.Length=0 then None else
            // ugly hack, cause I know the first group has 6 elts
            if halve=false && built.Length > minGroupSize then None else

            for idx in 0..(remaining.Length-1) do
                let elt = remaining[idx]
                let built2 = elt::built
                let remaining2 = remaining |> List.skip (idx+1)
                let r = built2,remaining2

                let total = totalSoFar + elt //built2 |> List.sum
                if total = quota then yield Some built2
                else if total < quota then pq.Enqueue(r, total)
    }
    |> Seq.choose id
    |> List.ofSeq
    |> List.map (fun built -> built, elts |> List.except built)

let solve1 (text:string) = 
    let inp = text |> parse2lines |> List.sortByDescending id
    let quota = inp |> List.sum |> fun x -> x/3
    let minGroupSize=6 // magic number

    let allF1A = fillUntil minGroupSize false quota inp
    let minLen = allF1A |> List.map (fst >> List.length) |> List.min
    let allF1B = allF1A |> List.filter (fun (built,_) -> built.Length=minLen)
    let allF3 = 
        allF1B
        |> Seq.sortBy (fun (elts,_) -> (List.length elts, quantumEntanglement elts))
        |> Seq.map (fun (builtPrev,remainingPrev) -> 
            let allNext = fillUntil minGroupSize true quota remainingPrev
            allNext
            |> map (fun (builtNext, remainingNext) ->
                builtPrev::builtNext::remainingNext::[]
            )
        )
        |> Seq.concat

    let idealFirstGroup =
        allF3
        //|> List.map (fun xs -> xs |> List.sortBy (fun elts -> List.length elts , quantumEntanglement elts))
        |> Seq.map List.head
        |> Seq.head

    quantumEntanglement idealFirstGroup


    
let solve2 (text:string) =
    let inp = text |> parse2lines |> List.sortByDescending id
    let quota = inp |> List.sum |> fun x -> x/4
    let minGroupSize=4 // magic number

    let allF1A = fillUntil minGroupSize false quota inp
    let minLen = allF1A |> List.map (fst >> List.length) |> List.min
    let allF1B = allF1A |> List.filter (fun (built,_) -> built.Length=minLen)
    let allF2 = 
        allF1B
        |> Seq.sortBy (fun (elts,_) -> (List.length elts, quantumEntanglement elts))
        |> Seq.map (fun (builtPrev,remainingPrev) -> 
            let allNext = fillUntil minGroupSize true quota remainingPrev
            allNext
            |> map (fun (builtNext, remainingNext) ->
                builtPrev::builtNext::[],remainingNext
            )
        )
        |> Seq.concat
    let allF3 =
        allF2
        |> Seq.map (fun (builtPrev,remainingPrev) -> 
            let allNext = fillUntil minGroupSize true quota remainingPrev
            allNext
            |> map (fun (builtNext, remainingNext) ->
                builtPrev@builtNext::remainingNext::[]
            )
        )
        |> Seq.concat

    let idealFirstGroup =
        allF3
        //|> List.map (fun xs -> xs |> List.sortBy (fun elts -> List.length elts , quantumEntanglement elts))
        |> Seq.map List.head
        //|> Seq.sortBy (fun elts -> (List.length elts, quantumEntanglement elts))
        |> Seq.head

    quantumEntanglement idealFirstGroup

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()