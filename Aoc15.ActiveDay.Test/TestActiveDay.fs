module Aoc15.ActiveDay.Test

open Aoc15.Input
open Aoc15.ActiveDay
open NUnit.Framework
open System

[<SetUp>]
let Setup () =
    ()
    
[<TestCase("input-TEST.txt")>] 
let Test1 (fn : string) = 
    let input = fn |> f2text
    let sln1 = solve1 input
    Console.WriteLine(sln1)
    Assert.Pass(sln1.ToString())

[<TestCase("input-TEST.txt")>] 
let Test2 (fn : string) = 
    let input = fn |> f2text
    let sln2 = solve2 input
    Console.WriteLine(sln2)
    Assert.Pass(sln2.ToString())
