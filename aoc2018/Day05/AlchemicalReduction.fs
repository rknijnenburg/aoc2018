module Day5.AlchemicalReduction

open System.IO
open System
open System.Text

let Parse(): string =
    File.ReadAllText("Day05/input.txt").Trim()

let ProcessPolymer(polymer: string): string = 
    let mutable units = polymer.ToCharArray();
    let mutable m = 0
    let mutable i = 1
    
    while i < units.Length do
        if abs((int) units[m] - (int) units[i]) = 32 then
            units.[m] <- '*'
            units.[i] <- '*'
            i <- i + 1
            while m > 0 && units.[m] = '*' do
                m <- m - 1
        else
            i <- i + 1
            m <- m + 1
        while m < units.Length && units.[m] = '*' do
            m <- m + 1
        if m = i then
            i <- m + 1

    (new string(units)).Replace("*", "")

let GetRemaining(polymer: string): int =
    ProcessPolymer(polymer).Length

let FindShortestPolymer(polymer: string): int =
    let mutable min = Int32.MaxValue

    for i = (int) 'a' to (int) 'z' do
        let c = ((char) i).ToString()
        min <- Math.Min(min, ProcessPolymer(polymer.Replace(c, "").Replace(c.ToUpper(), "")).Length)

    min

let Solve(): string = 
    let mutable builder = new StringBuilder();
    
    builder <- builder.AppendLine("Day 5: Alchemical Reduction");
    builder <- builder.AppendLine()

    let polymer = Parse();

    builder <- builder.AppendLine("How many units remain after fully reacting the polymer you scanned?")
    builder <- builder.AppendLine(sprintf "%i" (GetRemaining(polymer)))
    builder <- builder.AppendLine()

    builder <- builder.AppendLine("What is the length of the shortest polymer you can produce?")
    builder <- builder.AppendLine(sprintf "%i" (FindShortestPolymer(polymer)))
    builder <- builder.AppendLine()

    builder.ToString()