module Aoc15.ActiveDay

open Aoc15
open Input

let solve1 text = text + "_abc"
let solve2 text = text + "_bcd"

let res1 = "inputs\input.txt" |> f2text |> solve1

let finished = "a" + res1.ToString()
()