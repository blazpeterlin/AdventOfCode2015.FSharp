module Aoc15.ActiveDay.T03

open Aoc15.Input
open Aoc15.D03
open NUnit.Framework
open System

[<SetUp>]
let Setup () =
    ()
    
[<TestCase("D03\\input.txt", 2565)>] 
[<TestCase("D03\\input-TEST.txt", 2)>] 
let Test1 (fn : string, res: int) = 
    let input = fn |> f2text
    let sln1 = solve1 input
    Assert.That(sln1, Is.EqualTo res)
    
[<TestCase("D03\\input.txt", 2639)>] 
[<TestCase("D03\\input-TEST.txt", 11)>] 
let Test2 (fn : string, res: int) = 
    let input = fn |> f2text
    let sln2 = solve2 input
    Assert.That(sln2, Is.EqualTo res)
