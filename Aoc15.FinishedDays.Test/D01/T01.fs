module Aoc15.ActiveDay.T01

open Aoc15.Input
open Aoc15.D01
open NUnit.Framework
open System

[<SetUp>]
let Setup () =
    ()
    
[<TestCase("D01\\T01-example1.txt", -3)>] 
[<TestCase("D01\\T01-input.txt", 280)>] 
let ActualRun1 (fn : string) (result: int) = 
    let input = fn |> f2text
    let sln1 = solve1 input
    Assert.That(sln1, Is.EqualTo result)
    
[<TestCase("D01\\T01-example2.txt", 5)>] 
[<TestCase("D01\\T01-input.txt", 1797)>] 
let ActualRun2 (fn : string) (result: int) = 
    let input = fn |> f2text
    let sln2 = solve2 input
    Assert.That(sln2, Is.EqualTo result)
