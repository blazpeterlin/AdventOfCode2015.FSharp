module Aoc15.D11

open Aoc15
open Input
open TextCopy
open System.Collections.Generic
open Aoc15.Operators

let input =  "input.txt" |> f2text

let incrementSmart (pass0:string) = 
    
    let res = 
        [pass0.Length-1 .. -1 .. 0]
        |> fold (fun (pass: char array, stillInc: bool) i -> 
            if not stillInc then (pass, stillInc)
            else
                if pass[i]='z' 
                then 
                    pass[i] <- 'a'
                    (pass, true)
                else 
                    pass[i] <- pass[i] |> int |> (+)1 |> char
                    (pass, false)
        ) (pass0.ToCharArray(),true)
        |> fst
        |> System.String
    res.Replace('i','j').Replace('l','m').Replace('o','p')

let incrementSmartRec (pass0: string) = 
    let rec inc (pass:string) : string =
        let left = pass.Substring(0, pass.Length-1)
        match pass[pass.Length-1] with
        | 'z' -> 
            let next = inc left
            next + "a"
        | ch -> 
            let right = (ch |> int |> (+)1 |> char)
            left + right.ToString()

    let res = pass0 |> inc
    res.Replace('i','j').Replace('l','m').Replace('o','p')

        
let acceptPass (pass: string) =
   
    let cnd1 = pass.ToCharArray() |> ofArray |> windowed 3 |> List.exists (fun (a::b::c::[]) -> int a + 1 = int b && int b + 1 = int c)
    let cnd2 = (pass.Contains 'i' ||pass.Contains 'l' || pass.Contains 'o') |> not
    let cnd3 = 
        let wnds2 = pass.ToCharArray() |> ofArray |> windowed 2 |> List.filter (fun (a::b::[]) -> a=b) |> List.length
        let wnds3 = pass.ToCharArray() |> ofArray |> windowed 3 |> List.filter (fun (a::b::c::[]) -> a=b && b=c) |> List.length
        wnds2 >= 3 || (wnds2 = 2 && wnds3 = 0)

    cnd1 && cnd2 && cnd3

let parseLine (ln:string) =
    ln
    //|> text2tokens "x"
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> map parseLine
    |> head

let solve1 (text:string) = 
    let pass0 = text |> parse2lines
    pass0 
    |> Seq.unfold (fun st -> incrementSmartRec st |> fun s -> Some(s,s))
    |> Seq.find (fun pass -> acceptPass pass)
    
    

    
let solve2 (text:string) =
    let pass0 = text |> parse2lines
    pass0 
    |> Seq.unfold (fun st -> incrementSmartRec st |> fun s -> Some(s,s))
    |> Seq.filter (fun pass -> acceptPass pass)
    |> Seq.skip 1
    |> Seq.head

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()