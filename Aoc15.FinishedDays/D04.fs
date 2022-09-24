module Aoc15.D04

open Aoc15
open Input
open TextCopy
open System.Security.Cryptography
open System.Text

let input =  "input.txt" |> f2text

let MD5Hash (input : string) =
      use md5 = System.Security.Cryptography.MD5.Create()
      input
      |> System.Text.Encoding.ASCII.GetBytes
      |> md5.ComputeHash
      |> Seq.map (fun c -> c.ToString("X2"))
      |> Seq.reduce (+)

let parseLine (ln:string) =
    ln
    |> text2tokens "x"
    //|> List.map int
    //|> fun (x::y::z::[]) -> (x,y,z)

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine

let solve1 (text:string) = 
    Seq.initInfinite id
    |> Seq.filter (fun i -> MD5Hash (text+i.ToString()) |> fun x -> x.StartsWith "00000")
    |> Seq.head
    
let solve2 (text:string) =
    Seq.initInfinite id
    |> Seq.filter (fun i -> MD5Hash (text+i.ToString()) |> fun x -> x.StartsWith "000000")
    |> Seq.head

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()