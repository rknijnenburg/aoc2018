module Day12.SubterraneanSustainability

open System
open System.IO
open System.Text

type Rule = { Pattern: string; Result: char }
type Input = { InitialState: string; Rules: Rule[] }

let Parse(): Input =
    let lines = File.ReadAllLines("Day12/input.txt")
    { 
        InitialState = lines.[0].Replace("initial state: ", "").Trim(); 
        Rules = 
            lines.[2..] 
            |> Array.map (fun s -> s.Split("=>")) 
            |> Array.map (fun a -> { Pattern = a.[0].Trim(); Result = a.[1].Trim().[0] })
    }

let IsMatch( pots: Set<int>, index: int, c: char): bool =
    match c with
        | '#' -> pots.Contains(index)
        | _ -> pots.Contains(index) = false

let IsPatternMatch( pots: Set<int>, index: int, pattern: string): bool =
    let mutable found = IsMatch(pots, index, pattern[2])
    for i = 1 to 2 do
        found <- found && IsMatch(pots, index - i, pattern[2 - i])
        found <- found && IsMatch(pots, index + i, pattern[2 + i])
    found

let Format(pots: Set<int>) =
    let mutable builder = new StringBuilder()
    let min = (pots |> Seq.min) - 3
    let max = (pots |> Seq.max) + 3
    for i = -10 to min do
        builder <- builder.Append(".")
    for i = min to max do
        if (pots.Contains(i)) then
            builder <- builder.Append("#")
        else
            builder <- builder.Append(".")
    builder.ToString()

let GetSumPlantNumbers(input: Input, generations: int64): int64 =
    let mutable pots: Set<int> = input.InitialState.ToCharArray() |> Array.indexed |> Array.where (fun x -> snd x = '#') |> Array.map (fun x -> fst x) |> Set.ofSeq;
    let mutable g: int64 = 1L
    let mutable slope = 0;
    let mutable slopeCount = 0;

    while (slopeCount < 20 && g <= generations) do
      let mutable next: Set<int> = Set.ofSeq []
      let min = (pots |> Seq.min) - 3
      let max = (pots |> Seq.max) + 3
      for i = min to max do
        match input.Rules |> Seq.tryFind (fun x -> IsPatternMatch(pots, i, x.Pattern)) with 
        | Some rule -> 
            if rule.Result = '#' then 
                next <- next.Add(i)
        | None -> ()
      let sum = next |> Seq.sum
      let curSlope = sum - (pots |> Seq.sum)
      if (curSlope = slope) then
        slopeCount <- slopeCount + 1
      else
        slope <- curSlope
        slopeCount <- 0
      pots <- next
      g <- g + 1L
      
    if (slopeCount >= 20) then
        int64( (pots |> Seq.sum)) + (generations - (g - 1L)) * int64(slope)
    else
        int64( (pots |> Seq.sum))

let Solve(): string =
    let mutable builder = new StringBuilder()

    builder <- builder.AppendLine("Day 12: Subterranean Sustainability")
    builder <- builder.AppendLine()

    let input = Parse();

    builder <- builder.AppendLine("After 20 generations, what is the sum of the numbers of all pots which contain a plant?")
    builder <- builder.AppendLine(sprintf "%i" (GetSumPlantNumbers(input, 20L)))
    builder <- builder.AppendLine()
    
    builder <- builder.AppendLine("After fifty billion (50000000000) generations, what is the sum of the numbers of all pots which contain a plant?")
    builder <- builder.AppendLine(sprintf "%i" (GetSumPlantNumbers(input, 50000000000L)))
    builder <- builder.AppendLine()

    builder.ToString()