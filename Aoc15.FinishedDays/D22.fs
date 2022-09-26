module Aoc15.D22

open Aoc15
open Input
open TextCopy
open System.Collections.Generic
open Aoc15.Operators

let input =  "input.txt" |> f2text



type Effect = EfShield | EfPoison | EfRecharge
type Spell = SpMissile | SpDrain | SpShield | SpPoison | SpRecharge
type Char = { HP: int; Mana: int; Dmg: int; Armor: int; Effects: (Effect * int) list }

let parseLine (ln:string) =
    ln.Replace("Hit Points", "HP").Replace("Damage","Dmg")
    |> text2tokens ": "
    |> fun xs -> xs[0], int xs[1]
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine

let applyEffects (target:Char) =
    let t2 = 
        target.Effects
        |> List.fold (fun (tgt:Char) (eff,dur) ->
            match eff with 
            | EfShield -> { tgt with Armor=7 }
            | EfPoison -> { tgt with HP = tgt.HP - 3 }
            | EfRecharge -> { tgt with Mana = tgt.Mana + 101 }
            //| _ -> tgt
        ) { target with Armor=0 }
    { t2 with Effects=t2.Effects |> List.map (fun (x,y) -> x,y-1) |> List.filter (fun (x,y) -> y>0)}

let takeTurn (hard:bool) (manaSpent:int,(boss:Char),(plyr:Char)) =
    if boss.HP <= 0 || plyr.HP <= 0 then [] else

    let boss2 = boss |> applyEffects
    let plyr2A = plyr |> applyEffects

    let plyr2 = if hard then {plyr2A with HP=plyr2A.HP-1} else plyr2A
    
    if plyr2.HP <= 0 || plyr2.Mana < 53 then [] else
    // player turn
    let availableActions = 
        [
            if plyr2.Mana >=53 then Some SpMissile else None;
            if plyr2.Mana >=73 then Some SpDrain else None;
            if plyr2.Mana >=113 && boss2.Effects |> map fst |> List.contains EfPoison then None else Some SpPoison;
            if plyr2.Mana >=173 && plyr2.Effects |> map fst |> List.contains EfShield then None else Some SpShield;
            if plyr2.Mana >=229 && plyr2.Effects |> map fst |> List.contains EfRecharge then None else Some SpRecharge;
        ]
        |> List.choose id

    [ for act in availableActions do
        let manaSpentThisTurn,boss3,plyr3A =
            match act with
            | SpMissile -> 53,{ boss2 with HP=boss2.HP-4 },plyr2
            | SpDrain -> 73,{ boss2 with HP=boss2.HP-2 },{ plyr2 with HP=plyr2.HP+2;  }
            | SpShield -> 113,boss2,{ plyr2 with  Effects=(EfShield,6)::plyr2.Effects }
            | SpPoison -> 173,{ boss2 with Effects=(EfPoison,6)::boss2.Effects },plyr2
            | SpRecharge -> 229,boss2,{ plyr2 with  Effects=(EfRecharge,5)::plyr2.Effects }
        let totalManaSpent = manaSpent+manaSpentThisTurn
        let plyr3 = {plyr3A with Mana = plyr3A.Mana - manaSpentThisTurn}

        // boss turn

        let boss4 = boss3 |> applyEffects
        let plyr4 = plyr3 |> applyEffects

        let dmg = max (boss4.Dmg - plyr4.Armor) 1
        let boss5,plyr5 = boss4,{plyr4 with HP = plyr4.HP-dmg}

        totalManaSpent,boss5,plyr5
    ]

let setupStartingPQ text =
    let bossMap = text |> parse2lines |> Map.ofList
    let boss0 = { HP=bossMap["HP"]; Dmg=bossMap["Dmg"]; Armor=0; Mana=0; Effects =[]; }
    let plyr0 = { HP=50; Dmg=0; Armor=0; Mana=500; Effects=[]; }

    let st0 = 0,boss0,plyr0
    let pq = PriorityQueue()
    pq.Enqueue(st0,0)
    pq
    
let findLeastManaSpent (hard:bool) (pq:PriorityQueue<int*Char*Char,int>) =
    []
    |> Seq.unfold (fun closed -> 
        let state = pq.Dequeue()
        let newOnes = takeTurn hard state
        newOnes |> Seq.iter (fun (manaSpent,b,p) -> pq.Enqueue((manaSpent,b,p),manaSpent))
        Some(state,newOnes)
    )
    |> Seq.find (fun (manaSpent,boss,plyr) -> boss.HP<=0)
    |> fun (manaSpent,boss,plyr) -> manaSpent

let solve1 (text:string) = 
    let pq = text |> setupStartingPQ
    findLeastManaSpent false pq
    
let solve2 (text:string) =
    let pq = text |> setupStartingPQ
    findLeastManaSpent true pq

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()