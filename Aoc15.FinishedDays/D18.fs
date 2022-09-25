module Aoc15.D18

open Aoc15
open Input
open TextCopy
open System.Collections.Generic
open Aoc15.Operators

let input =  "input.txt" |> f2text

let parseLine (ln:string) =
    (ln).ToCharArray() |> ofArray |> map (function | '#' -> true | '.' -> false)
    // |> text2tokens "x"
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine

let step (keepCornersOn: bool) (m0 : Map<int*int,bool>) =
    let maxX = m0.Keys |> Seq.map fst |> Seq.max
    let maxY = m0.Keys |> Seq.map snd |> Seq.max

    let ignoredCorners = if keepCornersOn then [(0,0);(0,maxY-1);(maxX-1, 0);(maxX-1, maxY-1)] else []

    let countTrueNeighbours (x,y) =
        Seq.allPairs [x-1..x+1] [y-1..y+1]
        |> Seq.except [x,y]
        |> Seq.map (fun pos -> m0[pos])
        |> Seq.filter ((=)true)
        |> Seq.length

    let nextKVPs = 
        Seq.allPairs [0..maxX-1] [0..maxY-1]
        |> Seq.except ignoredCorners
        |> Seq.map (fun (x,y) -> 
            let nextVal = 
                match m0[x,y],(countTrueNeighbours (x,y)) with
                | true, v when v=2 || v=3 -> true
                | true, _ -> false
                | false, 3 -> true
                | false, _ -> false
            (x,y),nextVal
        )
    nextKVPs |> Seq.fold (fun (m:Map<int*int,bool>) kvp -> m.Add kvp) m0
    
let initM text = 
    let m0 = 
        text |> parse2lines 
        |> List.map (List.indexed) |> List.indexed
        |> List.map (fun (y,lst) -> lst |> List.map (fun (x, elt) -> (x,y),elt))
        |> List.concat
        |> Map.ofList
    let maxX = m0.Keys |> Seq.map fst |> Seq.max
    let maxY = m0.Keys |> Seq.map snd |> Seq.max
    let m0A = [-1..maxX+1] |> Seq.fold (fun m x -> m |> Map.add (x,-1) false |> Map.add (x,maxY+1) false) m0
    let m0B = [-1..maxY+1] |> Seq.fold (fun m y -> m |> Map.add (-1,y) false |> Map.add (maxX+1,y) false) m0A
    m0B


let solve1 (text:string) = 
    let m0 = initM text
    let mN = [1..100] |> Seq.fold (fun m i -> step false m) m0
    let numLights = mN.Values |> Seq.filter ((=)true) |> Seq.length
    numLights

    
let solve2 (text:string) =
    let m0 = initM text
    let maxX = m0.Keys |> Seq.map fst |> Seq.max
    let maxY = m0.Keys |> Seq.map snd |> Seq.max
    let m0B = m0 |> Map.add (0,0) true |> Map.add (0,maxY-1) true |> Map.add(maxX-1, 0) true |> Map.add (maxX-1, maxY-1) true
    let mN = [1..100] |> Seq.fold (fun m i -> step true m) m0B
    let numLights = mN.Values |> Seq.filter ((=)true) |> Seq.length
    numLights

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()