module Aoc15.ActiveDay.T25

open Aoc15.Input
open Aoc15.D25
open NUnit.Framework
open System

[<SetUp>]
let Setup () =
    ()

let basePath = System.Reflection.MethodInfo.GetCurrentMethod().DeclaringType.Name.Replace("T","D")
let prependFolder fname = basePath + "\\" + fname
    
[<TestCase("input.txt", 8997277)>] 
let Test1 (fn : string, res: int) = 
    let input = fn |> prependFolder |> f2text
    let sln1 = solve1 input
    Assert.That(sln1, Is.EqualTo res)
    
[<TestCase("Merry christmas!")>] 
let Test2 (str : string) = 
    Assert.That(str.Contains "Merry", Is.EqualTo true)
