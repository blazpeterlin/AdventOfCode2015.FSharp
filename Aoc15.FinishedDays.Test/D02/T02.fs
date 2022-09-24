module Aoc15.ActiveDay.T02

open Aoc15.Input
open Aoc15.D02
open NUnit.Framework
open System

[<SetUp>]
let Setup () =
    ()
    
[<TestCase("D02\\input.txt", 1606483)>] 
[<TestCase("D02\\input-TEST.txt", 43)>] 
let Test1 (fn : string, res: int) = 
    let input = fn |> f2text
    let sln1 = solve1 input
    Assert.That(sln1, Is.EqualTo res)
    
[<TestCase("D02\\input.txt", 3842356)>] 
[<TestCase("D02\\input-TEST.txt", 14)>] 
let Test2 (fn : string, res: int) = 
    let input = fn |> f2text
    let sln2 = solve2 input
    Assert.That(sln2, Is.EqualTo res)
