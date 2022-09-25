module Aoc15.D16

open Aoc15
open Input
open TextCopy
open System.Collections.Generic
open Aoc15.Operators

let input =  "input.txt" |> f2text

type Sue = { Id:int; Chi:int option; Cat: int option; Sam: int option; Pom: int option; Aki: int option; Viz: int option; Gol: int option; Tre: int option; Car: int option; Per: int option; }

let ourSueText = "Sue -1: children: 3, cats: 7, samoyeds: 2, pomeranians: 3, akitas: 0, vizslas: 0, goldfish: 5, trees: 3, cars: 2, perfumes: 1"

let parseLine (ln:string) =
    ln
    |> text2tokens " ,:"
    |> List.filter ((<>)"")
    |> fun xs ->
        let id = int xs[1]
        let kvps =
            [for idx in 2 .. 2 .. xs.Length-1 do
                yield xs[idx],int xs[idx+1]
            ]
        let map = kvps |> Map.ofList
        { 
            Id=id; 
            Chi=map.TryFind "children";
            Cat=map.TryFind "cats";
            Sam=map.TryFind "samoyeds";
            Pom=map.TryFind "pomeranians";
            Aki=map.TryFind "akitas";
            Viz=map.TryFind "vizslas";
            Gol=map.TryFind "goldfish";
            Tre=map.TryFind "trees";
            Car=map.TryFind "cars";
            Per=map.TryFind "perfumes";
        }
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine

let solve1 (text:string) = 
    let sues = text |> parse2lines
    let ourSue = ourSueText |> parseLine

    sues 
    |> List.find (fun sue -> 
        if sue.Chi.IsSome then sue.Chi = ourSue.Chi else true
        && if sue.Cat.IsSome then sue.Cat = ourSue.Cat else true
        && if sue.Sam.IsSome then sue.Sam = ourSue.Sam else true
        && if sue.Pom.IsSome then sue.Pom = ourSue.Pom else true
        && if sue.Aki.IsSome then sue.Aki = ourSue.Aki else true
        && if sue.Viz.IsSome then sue.Viz = ourSue.Viz else true
        && if sue.Gol.IsSome then sue.Gol = ourSue.Gol else true
        && if sue.Tre.IsSome then sue.Tre = ourSue.Tre else true
        && if sue.Car.IsSome then sue.Car = ourSue.Car else true
        && if sue.Per.IsSome then sue.Per = ourSue.Per else true
        )
    |> fun sue ->sue.Id
    
let solve2 (text:string) =
    let sues = text |> parse2lines
    let ourSue = ourSueText |> parseLine

    sues 
    |> List.find (fun sue -> 
        if sue.Chi.IsSome then sue.Chi = ourSue.Chi else true
        && if sue.Cat.IsSome then sue.Cat > ourSue.Cat else true
        && if sue.Sam.IsSome then sue.Sam = ourSue.Sam else true
        && if sue.Pom.IsSome then sue.Pom < ourSue.Pom else true
        && if sue.Aki.IsSome then sue.Aki = ourSue.Aki else true
        && if sue.Viz.IsSome then sue.Viz = ourSue.Viz else true
        && if sue.Gol.IsSome then sue.Gol < ourSue.Gol else true
        && if sue.Tre.IsSome then sue.Tre > ourSue.Tre else true
        && if sue.Car.IsSome then sue.Car = ourSue.Car else true
        && if sue.Per.IsSome then sue.Per = ourSue.Per else true
        )
    |> fun sue ->sue.Id

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()