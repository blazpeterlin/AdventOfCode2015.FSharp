module Aoc15.ActiveDay.T11

open Aoc15.Input
open Aoc15.D11
open NUnit.Framework
open System

[<SetUp>]
let Setup () =
    ()

let basePath = System.Reflection.MethodInfo.GetCurrentMethod().DeclaringType.Name.Replace("T","D")
let prependFolder fname = basePath + "\\" + fname
    
[<TestCase("input.txt", "cqjxxyzz")>] 
[<TestCase("input-TEST.txt", "abcdffaa")>] 
let Test1 (fn : string, res: string) = 
    let input = fn |> prependFolder |> f2text
    let sln1 = solve1 input
    Assert.That(sln1, Is.EqualTo res)
    
[<TestCase("input.txt", "cqkaabcc")>] 
let Test2 (fn : string, res: string) = 
    let input = fn |> prependFolder |> f2text
    let sln2 = solve2 input
    Assert.That(sln2, Is.EqualTo res)
