module Day5

open System.IO
open System

let ReadPolymer(): string =
    File.ReadAllText("Day5/input.txt").Trim()

let IsReaction(c1: char, c2: char): bool = 
     (Char.IsLower(c1) && Char.IsUpper(c2) && c1 = Char.ToLower(c2)) || (Char.IsUpper(c1) && Char.IsLower(c2) && (c1 = Char.ToUpper(c2)))

let TriggerBackwards(polymer: string, marker: int):  ( bool * string * int) = 
    let mutable result = false, polymer, marker
    if (marker > 0 && marker < polymer.Length) then
        let c1 = polymer.[marker-1]
        let c2 =  polymer.[marker]
        if (IsReaction(polymer.[marker], polymer.[marker-1])) then
           result <- true, polymer.Substring(0, marker - 1) + polymer.Substring(marker+1), marker - 2

    result
        

let TriggerForward(polymer: string, marker: int):  ( bool * string * int) = 
    let mutable result = false, polymer, marker
    if (marker > 0 && marker < (polymer.Length - 1)) then
        let c1 = polymer.[marker]
        let c2 =  polymer.[marker+1]
        if (IsReaction(polymer.[marker], polymer.[marker+1])) then
            result <- true, polymer.Substring(0, marker) + polymer.Substring(marker+2), marker - 1
    
    result


let TriggerNextUnit(polymer: string, marker: int): ( bool * string * int) =
    let mutable r, p, m = TriggerBackwards(polymer, marker)
    if r then
        true, p, m
    else
        let mutable r, p, m = TriggerForward(polymer, marker)
        if r then
            true, p, m            
        else
            false, polymer, marker

let ProcessPolymer(polymer: string): string = 
    let mutable marker = 0
    let mutable processed = polymer
    
    while marker < polymer.Length - 1 do
        let r, p, m = TriggerNextUnit(processed, marker)
        processed <- p
        marker <- m
        if not r then
            marker <- marker + 1

    processed

let CalculateUnitsRemaining: int =
    ProcessPolymer(ReadPolymer()).Length

let CalculateShortestPolymer: int =
    let polymer = ReadPolymer()
    let mutable min = Int32.MaxValue

    for i = 0 to 25 do
        let lower = Convert.ToChar(Convert.ToInt32('a') + i).ToString()
        let upper = Convert.ToChar(Convert.ToInt32('A') + i).ToString()
        min <- Math.Min(min, ProcessPolymer(polymer.Replace(lower, "").Replace(upper, "")).Length)

    min

let Solve: string = 
    sprintf "Day 5\n" +
    sprintf "How many units remain after fully reacting the polymer you scanned? %i\n" CalculateUnitsRemaining +
    sprintf "What is the length of the shortest polymer you can produce? %i\n" CalculateShortestPolymer