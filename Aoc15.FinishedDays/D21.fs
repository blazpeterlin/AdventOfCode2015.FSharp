module Aoc15.D21

open Aoc15
open Input
open TextCopy
open System.Collections.Generic
open Aoc15.Operators


type Char = { HP: int; Dmg: int; Armor: int }


type Item = { Name: string; Cost: int; Damage: int; Armor: int; }

let (+++) (ch:Char) (itm:Item) =
    { HP= ch.HP; Dmg= ch.Dmg + itm.Damage; Armor=ch.Armor+itm.Armor; }

let data2item (nm,(c,d,a)) = { Name=nm; Cost=c; Damage=d; Armor=a; }

let parseLine (ln:string) =
    ln.Replace("Hit Points", "HP").Replace("Damage","Dmg")
    |> text2tokens ": "
    |> fun xs -> xs[0], int xs[1]
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine

let calcCost ((weapon:Item),(armor:Item),(rings:Item list)) =
    weapon.Cost + armor.Cost + (rings |> map (fun r -> r.Cost) |> List.sum)

let calcPlayer (weapon:Item) (armor:Item) (rings:Item list) =
    let plyr = { HP=100; Dmg=0; Armor=0; }
    weapon::armor::rings
    |> fold (fun p itm -> p +++ itm) plyr

let victimAfterHit (victim:Char) (attacker:Char) =
    let dmg = max (attacker.Dmg - victim.Armor) 1
    { victim with HP = victim.HP - dmg }

let simulateBattleSuccess boss0 (w,a,r) =
    let plyr0 = calcPlayer w a r

    let plyrFinal,bossFinal = 
        (plyr0,boss0)
        |> Seq.unfold (fun (plyr,boss) -> 
            if boss.HP <= 0 || plyr.HP <= 0 then None else
            let boss2 = victimAfterHit boss plyr
            let plyr2 = victimAfterHit plyr boss
            let r = (plyr2,boss2)
            Some(r,r)
        )
        |> Seq.last
    bossFinal.HP <= 0

let setupPlayerConfigs =
    let shopWeapons = [
        "Dagger", (8, 4, 0) ;
        "Shortsword", (10, 5, 0) ;
        "Warhammer", (25, 6, 0 );
        "Longsword", (40, 7, 0) ;
        "Greataxe", (74, 8, 0 );
        ]
    let shopArmors = [
        "Leather", (13, 0, 1) ;
        "Chainmail", (31, 0, 2) ;
        "Splintmail", (53, 0, 3) ;
        "Bandedmail", (75, 0, 4) ;
        "Platemail", (102, 0, 5) ;
    ]
    let shopRings = [
        "Damage +1", (25, 1, 0) ;
        "Damage +2", (50, 2, 0) ;
        "Damage +3", (100, 3, 0) ;
        "Defense +1", (20, 0, 1) ;
        "Defense +2", (40, 0, 2) ;
        "Defense +3", (80, 0, 3) ;
    ]
   
    let weapons = shopWeapons |> map data2item
    let armors = shopArmors |> map data2item
    let rings = shopRings |> map data2item

    let noArmor = { Name="naked"; Damage=0; Armor=0; Cost=0; }
    let noRings = { Name="0rings"; Damage=0; Armor=0; Cost=0; }

    let weaponConfigs = weapons
    let armorConfigs = noArmor :: armors
    let zeroRings = [[noRings]]
    let oneRing = rings|>map (fun r -> [r])
    let twoRings = List.allPairs rings rings |> List.filter (fun (x,y) -> x<>y && x.Cost <= y.Cost) |> List.map (fun (x,y) -> x::y::[])
    let ringConfigs = zeroRings@oneRing@twoRings
    
    let allPlayerConfigs = 
        List.allPairs weaponConfigs armorConfigs
        |> map (fun (w,a) -> ringConfigs |> map (fun rs -> w,a,rs))
        |> List.concat
    allPlayerConfigs


let solve1 (text:string) = 
    let bossMap = text |> parse2lines |> Map.ofList
    let boss = { HP=bossMap["HP"]; Dmg=bossMap["Dmg"]; Armor=bossMap["Armor"] }

    let allPlayerConfigs = setupPlayerConfigs

    let lowestEquip =
        allPlayerConfigs
        |> List.sortBy calcCost
        |> Seq.choose (fun cfg -> 
            if simulateBattleSuccess boss cfg then Some cfg else None
        )
        |> Seq.head

    lowestEquip |> calcCost

    
let solve2 (text:string) =
    let bossMap = text |> parse2lines |> Map.ofList
    let boss = { HP=bossMap["HP"]; Dmg=bossMap["Dmg"]; Armor=bossMap["Armor"] }

    let allPlayerConfigs = setupPlayerConfigs

    let highestEquip =
        allPlayerConfigs
        |> List.sortByDescending calcCost
        |> Seq.choose (fun cfg -> 
            if simulateBattleSuccess boss cfg then None else Some cfg
        )
        |> Seq.head

    highestEquip |> calcCost
    
// not sure why input insisted on loading from tests on specifically this day
if System.IO.File.Exists "input.txt" |> not then () |> ignore else
let input =  "input.txt" |> f2text

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
() |> ignore