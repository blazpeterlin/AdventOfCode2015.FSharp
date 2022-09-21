module Aoc15.ActiveDay.Test

open Aoc15.Input
open Aoc15.ActiveDay
open NUnit.Framework
open System

[<SetUp>]
let Setup () =
    ()

let Execute (solve:string->string) file =
     let solved = file |> f2text |> solve

     Console.WriteLine(solved)
     Assert.Pass()

let Execute1 file = Execute solve1 file
let Execute2 file = Execute solve2 file

[<Test>] 
let Test1 () = Execute1 "inputs\\input.txt"

[<Test>] 
let Run1 () = Execute1 "inputs\\input-test.txt"

[<Test>] 
let Test2 () = Execute2 "inputs\\input.txt"

[<Test>] 
let Run2 () = Execute2 "inputs\\input-test.txt"
