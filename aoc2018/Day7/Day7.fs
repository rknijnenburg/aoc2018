module Day7

open System
open System.IO

let ReadInstructions(): string[] =
    File.ReadAllLines("Day7/input.txt")

let GetNextStep(map: Map<char, Set<char>>): char option =
    Map.tryFindKey (fun _ value -> (Set.count value) = 0) map

let GetNextSteps(map: Map<char, Set<char>>): Set<char> =
    map |> Map.filter (fun key value -> (Set.count value) = 0) |> Map.toSeq |> Seq.map fst |> Set.ofSeq

let BuildMap(): Map<char, Set<char>> =
    let mutable map : Map<char, Set<char>> = Map.ofSeq[]

    for instruction in ReadInstructions() do
        let split = instruction.Split(" ")
        let step = split.[7].[0]
        let required = split.[1].[0]
        if not (map.ContainsKey(step)) then
            map <- map.Add(step, Set.ofSeq [])
        if not (map.ContainsKey(required)) then
            map <- map.Add(required, Set.ofSeq [])
        map <- map.Add(step, Set.add required map.[step])

    map

let CalculateCorrectOrder: string = 
    let mutable map = BuildMap()
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

let CalculateTimeToComplete: int = 
    let mutable map = BuildMap()
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

let Solve: string =
    sprintf "Day 7\n" +
    sprintf "In what order should the steps in your instructions be completed?  %s\n" CalculateCorrectOrder +
    sprintf "How long will it take to complete all of the steps? %i\n" CalculateTimeToComplete