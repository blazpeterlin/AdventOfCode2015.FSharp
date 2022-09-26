module Aoc15.D19

open Aoc15
open Input
open TextCopy
open System.Collections.Generic
open Aoc15.Operators

let input =  "input.txt" |> f2text

let parseLine (ln:string) =
    ln
    |> text2tokens " =>"
    |> fun xs -> xs[0], xs[1]
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    let strs = text |> text2lines 
    let splitIdx = strs |> List.findIndex (fun s -> s="")
    let molecule = strs[splitIdx+1]
    molecule, strs |> List.take splitIdx |> List.map parseLine

let mol2list (molecule:string) =
    molecule.ToCharArray()
    |> ofArray
    |> List.rev
    |> fold (fun (lastCh, acc) ch -> 
        if System.Char.IsLower(ch) && ch <> 'e' then (Some ch, acc) 
        else if lastCh.IsSome then (None, (ch.ToString() + lastCh.Value.ToString())::acc)
        else (None, ch.ToString()::acc)
    ) (None, [])
    |> snd

let substitutions  (subsDict:Map<string,(string*string) list>) (elt: string) =
    if (subsDict.ContainsKey elt |> not) then [] else
    subsDict[elt] |> List.map snd

let singleStep subsDict molList =
    let distincts = 
        molList 
        |> List.mapi (fun i elt ->
            let prefix = molList[0..i-1] |> List.fold (+) ""
            let postfix = molList[i+1..molList.Length-1] |> List.fold (+) ""
            let subs = substitutions subsDict elt
            subs |> List.map (fun sub -> prefix + sub + postfix)
            )
        |> List.concat
        |> distinct
    distincts

let solve1 (text:string) = 
    let molecule, subs = text |> parse2lines
    let subsDict = subs |> List.groupBy fst |> Map.ofList
    let molList = molecule |> mol2list
    
    let distincts = singleStep subsDict molList
    distincts |> length

let getAllIdxOf (substr:string) (str:string) : int seq =
    -1
    |> Seq.unfold (fun i ->
        let nextIdx = str.IndexOf(substr, i+1)
        if nextIdx = -1 then None else Some(nextIdx,nextIdx)
    )

//let smartStepBack (keysByLen: string list) (subsDictBack:Map<string,string list>) ((steps,molecule):int*string) =
//    let firstGoodKey = keysByLen |> Seq.tryFind (fun key -> (molecule.IndexOf key) >= 0)
//    if firstGoodKey = None then Seq.empty else 
//    let key = firstGoodKey.Value

//    seq {
//        for idx in molecule |> getAllIdxOf key do
//            let prefix = molecule.Substring(0, idx)
//            let postfix = molecule.Substring(idx+key.Length, molecule.Length-idx-key.Length)
//            let lenOther = prefix.Length+postfix.Length
//            for res in subsDictBack[key] |> map (fun x -> (lenOther+x.Length),(steps+1,prefix+x+postfix)) do yield res
//    }

    
let solve2 (text:string) =
    let molecule, subs = text |> parse2lines

    // idea shamelessly grabbed from https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/cy4etju/
    let mol0 = 
        molecule.Replace("Rn", "(").Replace("Ar",")").Replace("Y",",").ToCharArray()
        |> Seq.filter (fun ch -> System.Char.IsLower(ch) |> not)
        |> Seq.map (fun ch -> if System.Char.IsUpper(ch) then 'X' else ch)
        |> Seq.toArray
        |> System.String

    let reduceMol (numSteps:int, mol:string) = 
        (numSteps, mol)
        |> Seq.unfold (fun (count, str) -> 
            if str.Contains("XX") then
                let strNext = str.Replace("XX", "X")
                let countNext = count + (str.Length - strNext.Length)
                let r = (countNext,strNext)
                Some(r,r)
            else if str.Contains("X(X)") then
                let strNext = str.Replace("X(X)", "X")
                let countNext = count + (str.Length - strNext.Length)/3
                let r = (countNext,strNext)
                Some(r,r)
            else if str.Contains("X(X,X)") then
                let strNext = str.Replace("(X,X)", "(X)")
                //let countNext = count + (str.Length - strNext.Length)/5
                let r = (count,strNext)
                Some(r,r)
            else if str.Contains("X(X,X,X)") then
                let strNext = str.Replace("(X,X,X)", "(X)")
                //let countNext = count + (str.Length - strNext.Length)/7
                let r = (count,strNext)
                Some(r,r)
            else None
        )
        |> Seq.last

    let (stepsN,molN) = (0,mol0) |> reduceMol

    // 243 too high
    // 229 too high

    stepsN

    //let flip (x,y) = (y,x)
    //let subsDictBack = subs |> map flip |> Common.groupByKeyVal fst snd |> Map.ofList
    //let keysByLen = subsDictBack.Keys |> List.ofSeq |> List.sortByDescending (fun x -> x.Length)
    //let ssb = smartStepBack keysByLen subsDictBack

    //let pq = PriorityQueue<int*string,int>()
    //pq.Enqueue((0,molecule),molecule.Length)
    //let closed = HashSet<string>()

    //let res = 
    //    Seq.initInfinite id
    //    |> Seq.map (fun _ ->
            
    //        let head = pq.Dequeue()
    //        let headStr = snd head
    //        if closed.Contains headStr then [] else
    //        let nextElts = head |> ssb |> List.ofSeq
    //        nextElts |> Seq.iter (fun (prio,elt)-> pq.Enqueue(elt, prio))
    //        closed.Add headStr |> ignore

    //        let forOutput = nextElts |> map snd
    //        forOutput
    //    )
    //    |> Seq.find (fun lst -> lst |> map snd |> List.contains "e")
    //    |> List.find (fun (steps,elt) -> elt="e")
    //    |> fst

    //res

    

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()