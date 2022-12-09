module Day1.ChronalCalibration

open System
open System.IO
open System.Text


let Parse() : int [] = 
    File.ReadAllLines("Day1/input.txt") |> Array.map (fun x -> Int32.Parse(x))

let CalculateFrequency(modifications: int[]) : int = 
     modifications |> Array.fold (fun acc elem -> acc + elem) 0

let FindFirstDuplicateFrequency(modifications: int[]): int =
    let mutable history : Set<int> = set []
    let mutable position = 0
    let mutable frequency = 0

    while position >= 0 do
        frequency <- frequency + modifications.[position]
        if history.Contains(frequency) then
            position <- -1
        else
            position <- position + 1
            if position >= modifications.Length then
                position <- 0
        
        history <- history.Add(frequency)

    frequency

let Solve(): string = 
    let mutable builder = new StringBuilder();
    
    builder <- builder.AppendLine("Day 1: Chronal Calibration");
    builder <- builder.AppendLine()

    let modifications = Parse();

    builder <- builder.AppendLine("what is the resulting frequency?")
    builder <- builder.AppendLine(sprintf "%i" (CalculateFrequency(modifications)))
    builder <- builder.AppendLine()

    builder <- builder.AppendLine("What is the first frequency your device reaches twice?")
    builder <- builder.AppendLine(sprintf "%i" (FindFirstDuplicateFrequency(modifications)))
    builder <- builder.AppendLine()

    builder.ToString()
    
    

