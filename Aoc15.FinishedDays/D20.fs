module Aoc15.D20

open Aoc15
open Input
open TextCopy
open System.Collections.Generic
open Aoc15.Operators

let input =  "input.txt" |> f2text

let solve1 (text:string) = 
    let inputNum = text |> int64

    let minCandidate = 1L
    let maxCandidate = inputNum/40L

    let bestHouse = 
        seq { 
            for elf in seq { 1L..maxCandidate } do
                let realMin = ((minCandidate-1L)/elf+1L)*elf
                let realMax = (maxCandidate/elf)*elf
                for house in [realMin .. elf .. realMax] do
                    let res = (house, elf*10L)
                    res
        }
        |> Common.seqGroupByKeyVal fst snd
        |> Seq.map (fun (k,v) -> (k,v |> Seq.sum))
        |> Seq.filter (fun (k,v) -> v >= inputNum)
        |> Seq.sort
        |> Seq.head
        |> fst
    bestHouse
    
let solve2 (text:string) =
    let inputNum = text |> int64

    let minCandidate = 1L
    let maxCandidate = inputNum/40L

    let bestHouse = 
        seq { 
            for elf in seq { 1L..maxCandidate } do
                let realMin = ((minCandidate-1L)/elf+1L)*elf
                let realMax = min ((maxCandidate/elf)*elf) (elf*40L)
                for house in [realMin .. elf .. realMax] do
                    let res = (house, elf*11L)
                    res
        }
        |> Common.seqGroupByKeyVal fst snd
        |> Seq.map (fun (k,v) -> (k,v |> Seq.sum))
        |> Seq.filter (fun (k,v) -> v >= inputNum)
        |> Seq.sort
        |> Seq.head
        |> fst
    bestHouse
    // 720720 too low

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()