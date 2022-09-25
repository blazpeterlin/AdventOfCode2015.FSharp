module Aoc15.D13

open Aoc15
open Input
open TextCopy
open System.Collections.Generic
open Aoc15.Operators

let input =  "input.txt" |> f2text

type State = { XFrom:string; XTo:string; Happiness:int; }

let calcScore (dict:Dictionary<string*string,int>) (persons:string list) =
    let persons2 = (List.last persons) :: persons
    persons2 |> List.pairwise |> map (fun (x,y) -> if x="Me"||y="Me" then 0 else dict[x,y]+dict[y,x]) |> sum

let states2dict (sts:State list) =
    sts
    |> fold (fun (dict:Dictionary<string*string,int>) st -> 
            dict[(st.XFrom,st.XTo)] <- st.Happiness
            dict
        )
        (Dictionary<string*string,int>())

let parseLine (ln:string) =
    ln
    |> text2tokens " ."
    |> fun xs -> 
        let diff = int xs[3] |> fun x -> if xs[2] = "gain" then x else -x
        { XFrom=xs[0]; XTo=xs[10]; Happiness=diff }
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine

let calcPersons (sts: State list) =
    (sts |> map (fun st -> st.XFrom))
        |> List.append (sts |> map (fun st -> st.XTo))
        |> distinct

let calcBestScore dict persons = 
    let allPossibleStates = 
            persons 
            |> Common.generateCombinationsNonRepeating
    let bestScore =
        allPossibleStates
        |> map (calcScore dict)
        |> List.max
    bestScore

let solve1 (text:string) = 
    let sts = text |> parse2lines
    let dict = sts |> states2dict
    let persons = calcPersons sts
    calcBestScore dict persons
    
let solve2 (text:string) =
    let sts = text |> parse2lines
    let dict = sts |> states2dict
    let persons = calcPersons sts |> List.append ["Me"]
    calcBestScore dict persons

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()