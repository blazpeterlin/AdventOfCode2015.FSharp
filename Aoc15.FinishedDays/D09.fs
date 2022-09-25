module Aoc15.D09

open Aoc15
open Input
open TextCopy
open System.Collections.Generic
open Operators

let input =  "input.txt" |> f2text

let parseLine (ln:string) =
    ln
    |> text2tokensStr [" to ";" = "]
    |> fun lst -> (lst[0], lst[1], int lst[2])
    //|> List.map int
    //|> fun (x::y::z::[]) -> (x,y,z)

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine

let getAllDists (text:string) =
    let tpls = text |> parse2lines
    let cities = 
        tpls |> List.map (fun (x,_,_) -> x)
        |> List.append (tpls |> List.map (fun (_,x,_) -> x))
        |> List.distinct

    let dict0 = Dictionary<string*string,int>()
    let dict =
        tpls
        |> Seq.fold
            (fun (dict:Dictionary<string*string,int>) (xFrom,xTo,dist) ->
                dict[(xFrom,xTo)] <- dist
                dict[(xTo,xFrom)] <- dist
                dict
            )
            dict0

    let candidatePaths = cities |> Common.generateCombinationsNonRepeating

    let dists = 
        candidatePaths
        |> map (fun path ->
            path
            |> List.pairwise
            |> map ( fun (a,b) -> dict[a,b] )
            |> sum
        )
    dists

let solve1 (text:string) = 
    let dists = getAllDists text
    List.min dists
    
let solve2 (text:string) =
    let dists = getAllDists text
    List.max dists

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()