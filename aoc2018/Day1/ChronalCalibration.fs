module Day1

open System
open System.IO
open System.Text


let ReadFrequencyModifications() : int [] = 
    File.ReadAllLines("Day1/input.txt") |> Array.map (fun x -> Int32.Parse(x))

let CalculateFrequency : int = 
     ReadFrequencyModifications() |> Array.fold (fun acc elem -> acc + elem) 0

let CalculateFirstDuplicateFrequency: int =
    let modifications = ReadFrequencyModifications()
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

let Solve: string = 
    "Day 1\n" +
    sprintf "What is the resulting frequency?: %i\n" CalculateFrequency +
    sprintf "What is the first frequency your device reaches twice?: %i\n" CalculateFirstDuplicateFrequency
    
    

