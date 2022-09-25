module Aoc15.ActiveDay.T15

open Aoc15.Input
open Aoc15.D15
open NUnit.Framework
open System

[<SetUp>]
let Setup () =
    ()

let basePath = System.Reflection.MethodInfo.GetCurrentMethod().DeclaringType.Name.Replace("T","D")
let prependFolder fname = basePath + "\\" + fname
    
[<TestCase("input.txt", 21367368)>] 
[<TestCase("input-TEST.txt", 62842880)>] 
let Test1 (fn : string, res: int) = 
    let input = fn |> prependFolder |> f2text
    let sln1 = solve1 input
    Assert.That(sln1, Is.EqualTo res)
    
[<TestCase("input.txt", 1766400)>] 
[<TestCase("input-TEST.txt", 57600000)>] 
let Test2 (fn : string, res: int) = 
    let input = fn |> prependFolder |> f2text
    let sln2 = solve2 input
    Assert.That(sln2, Is.EqualTo res)
