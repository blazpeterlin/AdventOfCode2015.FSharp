module Aoc15.D17

open Aoc15
open Input
open TextCopy
open System.Collections.Generic
open Aoc15.Operators

let input =  "input.txt" |> f2text

let parseLine (ln:string) =
    int ln
    // |> text2tokens "x"
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine

let solve1 (text:string) = 
    let conts = text |> parse2lines
    let combs0 = [[]]
    let nextCombs combs i =
        let appended = combs |> map (fun c -> conts[i]::c)
        appended@combs
    let allCombinations = [0..conts.Length-1] |> List.fold nextCombs combs0
    allCombinations
    |> map sum
    |> filter ((=)150)
    |> length
    
let solve2 (text:string) =
    let conts = text |> parse2lines
    let combs0 = [[]]
    let nextCombs combs i =
        let appended = combs |> map (fun c -> conts[i]::c)
        appended@combs
    let allCombinations = [0..conts.Length-1] |> List.fold nextCombs combs0
    
    let fitting = 
        allCombinations
        |> filter (fun x -> sum x = 150)
        |> List.sortBy (fun x -> x.Length)

    let smallOnes = 
        fitting
        |> filter (fun x -> x.Length = fitting[0].Length)
    
    smallOnes.Length

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()