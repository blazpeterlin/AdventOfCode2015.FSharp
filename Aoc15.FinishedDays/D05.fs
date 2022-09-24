module Aoc15.D05

open Aoc15
open Input
open TextCopy

let input =  "input.txt" |> f2text

let isNice (str:string) =
    let lst = str.ToCharArray() |> List.ofArray
    let vowels = 
        lst
        |> List.filter (fun x -> Array.contains x ("aeiou".ToCharArray()))
        |> List.length
        |> fun l -> l>=3

    let twice = lst |> List.windowed 2 |> List.filter (fun (x::y::[]) -> x=y) |> List.isEmpty |> not

    let noBads = 
        ["ab";"cd";"pq";"xy"]
        |> List.map (fun bad -> str.IndexOf bad)
        |> List.max
        |> (=) -1
    vowels && twice && noBads

let isNice2 (str:string) =
    let lst = str.ToCharArray() |> List.ofArray

    let cnd1 =
        lst
        |> List.windowed 2
        |> List.indexed
        |> List.filter (fun (idx,a::b::[]) -> str.Substring(idx+2).IndexOf(a.ToString()+b.ToString()) >= 0)
        |> List.isEmpty |> not

    let cnd2 = 
        lst
        |> List.windowed 3
        |> List.filter (fun (a::b::c::[]) -> a=c)
        |> List.isEmpty |> not

    cnd1 && cnd2



let parseLine (ln:string) =
    ln
    //|> List.map int
    //|> fun (x::y::z::[]) -> (x,y,z)

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine

let solve1 (text:string) =
    text |> parse2lines |> List.map isNice |> List.filter ((=)true) |> List.length
    
let solve2 (text:string) =
    text |> parse2lines |> List.map isNice2 |> List.filter ((=)true) |> List.length
    

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()