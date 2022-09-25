module Aoc15.D15

open Aoc15
open Input
open TextCopy
open System.Collections.Generic
open Aoc15.Operators

let input =  "input.txt" |> f2text

type Ingredient = { Cap: int; Dur: int; Flav: int; Text: int; Cal: int; }

let (+++) (ing1:Ingredient) (ing2:Ingredient) =
    { Cap=ing1.Cap+ing2.Cap; Dur=ing1.Dur+ing2.Dur; Flav=ing1.Flav+ing2.Flav; Text=ing1.Text+ing2.Text; Cal=ing1.Cal+ing2.Cal; }

let parseLine (ln:string) =
    ln
    // |> text2tokens "x"
    |> text2tokensStr [": capacity ";", durability ";", flavor ";", texture ";", calories "]
    |> fun xs -> { Cap=int xs[1]; Dur=int xs[2]; Flav = int xs[3]; Text = int xs[4]; Cal = int xs[5] }


let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine

let score ings nums =
    let res = 
        List.zip ings nums
        |> map (fun (ing, num) -> { Cap=ing.Cap*num; Dur=ing.Dur*num; Flav=ing.Flav*num; Text=ing.Text*num; Cal=ing.Cal*num; })
        |> List.reduce (+++)
    if res.Cap < 0 || res.Dur < 0 ||res.Flav < 0 || res.Text < 0 then (0L, 0) else
    ((int64 res.Cap) * (int64 res.Dur) * (int64 res.Flav) * (int64 res.Text), res.Cal)

let genPossibilities total numDims = 
    let nums0 = [[]]
    let numsNext nums =
        let numSum = nums |> sum    
        [for r in 0..total-numSum do
            yield r::nums]
    let almostFinalNums = [1..numDims-1] |> fold (fun st iter -> st |> map numsNext |> List.concat) nums0
    almostFinalNums |> map (fun nums -> (total - sum nums)::nums)
    


let solve1 (text:string) = 
    let ings = text |> parse2lines
    
    let total=100

    let possibilities = genPossibilities 100 ings.Length
    let bestScore = 
        possibilities
        |> Seq.map (fun nums -> score ings nums |> fst)
        |> Seq.max
    bestScore
    
let solve2 (text:string) =
    let ings = text |> parse2lines
    
    let total=100
    
    let possibilities = genPossibilities 100 ings.Length
    let bestScore = 
        possibilities
        |> Seq.map (fun nums -> score ings nums)
        |> Seq.filter (fun (_,cals) -> cals=500)
        |> Seq.map fst
        |> Seq.max
    bestScore

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()