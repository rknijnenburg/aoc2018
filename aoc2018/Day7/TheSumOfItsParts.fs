module Day7.TheSumOfItsParts

open System
open System.IO
open System.Text
open System.Linq

type Rule = { Required: char; Begin: char }

let Parse(): Rule[] =
    File.ReadAllLines("Day7/input.txt") 
        |> Array.map (fun x -> 
            let slices = x.Split(" ")
            { Required = slices.[1].[0]; Begin = slices.[7].[0] }
        )

let GetNextStep(map: Map<char, Set<char>>): char option =
    Map.tryFindKey (fun _ value -> (Set.count value) = 0) map

let GetNextSteps(map: Map<char, Set<char>>): Set<char> =
    map |> Map.filter (fun key value -> (Set.count value) = 0) |> Map.toSeq |> Seq.map fst |> Set.ofSeq    

let BuildMap(rules: Rule[]): Map<char, Set<char>> =
    let mutable map: Map<char, Set<char>> =
        rules
        |> Array.map (fun x -> [| x.Required; x.Begin |])
        |> Array.concat
        |> Array.distinct
        |> Array.map (fun x->  (x, Set.ofSeq []))
        |> Map.ofSeq

    for rule in rules do
        map <- map.Add(rule.Begin, Set.add rule.Required map.[rule.Begin])

    map


let FindCorrectOrder(rules: Rule[]): string = 
    let mutable map = BuildMap(rules)
    let mutable step = GetNextStep(map)
    let mutable result = String.Empty

    while step.IsSome do
        result <- result + (string step.Value)
        map <- map.Remove(step.Value)
        for pair in map do
            map <- Map.add pair.Key (pair.Value.Remove(step.Value)) map
        step <- GetNextStep(map)

    result

type Worker = { mutable Task: char option; mutable Remaining: int }

let CalculateTimeToComplete(rules: Rule[]): int = 
    let mutable map = BuildMap(rules)
    let mutable workers = [0..4] |> Seq.map (fun w -> { Task = None; Remaining = 0 }) |> Seq.toArray
    let mutable duration = 0
    let mutable finished = []
    
    while (map.Count > 0 || (Array.tryFind (fun x -> x.Task <> None) workers).IsSome) do
        let mutable steps = GetNextSteps(map)

        for i = 0 to workers.Length - 1 do
            let worker = workers.[i]
            if worker.Remaining = 0 && steps.Count > 0 then
                    let step = steps |> Set.toList |> List.item 0
                    worker.Task <- Some step
                    worker.Remaining <- 60 + int step - int 'A' + 1
                    steps <- Set.remove step steps
                    map <- Map.remove step map
                    
            if (worker.Remaining > 0) then
                worker.Remaining <- worker.Remaining - 1
            if worker.Remaining = 0 && (worker.Task.IsSome) then
                finished <- List.append finished [worker.Task.Value]
                for pair in map do
                    map <- Map.add pair.Key (pair.Value.Remove(worker.Task.Value)) map
                worker.Task <- None
                Array.set workers i worker
            Array.set workers i worker
        duration <- duration + 1
        
    duration

let Solve(): string =
    let mutable builder = new StringBuilder();
    
    builder <- builder.AppendLine("Day 7: The Sum of its Parts");
    builder <- builder.AppendLine()

    let rules = Parse();

    builder <- builder.AppendLine("In what order should the steps in your instructions be completed?")
    builder <- builder.AppendLine(sprintf "%s" (FindCorrectOrder(rules)))
    builder <- builder.AppendLine()

    builder <- builder.AppendLine("How long will it take to complete all of the steps?")
    builder <- builder.AppendLine(sprintf "%i" (CalculateTimeToComplete(rules)))
    builder <- builder.AppendLine()

    builder.ToString()