﻿module Day4.ReposeRecord

open System.IO
open System
open System.Globalization
open System.Linq
open System.Text

let Parse(): Map<int, int[]> =
    let records = File.ReadAllLines("Day4/input.txt") |> Array.map (fun x -> (DateTime.ParseExact(x.Substring(1, 16), "yyyy-MM-dd HH:mm", CultureInfo.InvariantCulture) , x.Substring(19))) |> Map.ofSeq
    let mutable id = -1
    let mutable schedule : Map<int, int[]> = [] |> Map.ofSeq
    let mutable start = DateTime.MinValue
    let mutable finish = DateTime.MinValue
    
    for record in records do
        if (record.Value.StartsWith("Guard #")) then
            id <- Int32.Parse(record.Value.Split(" ").[1].Replace("#", ""))
            if not (schedule.ContainsKey(id)) then
                schedule <- schedule.Add(id, ([|0..59|] |> Array.map (fun x -> 0 )))
        if (record.Value = "falls asleep") then
            start <- record.Key
        if (record.Value = "wakes up") then
            finish <- record.Key
            for offset = 0 to int (finish - start).TotalMinutes - 1 do
                let minute = start.AddMinutes(float offset).Minute
                Array.set schedule.[id] minute (schedule.[id].[minute] + 1)

    schedule   


let FindGuardMostMinutesAsleep(schedule: Map<int, int[]>): int =
    let guard, sum = schedule.Select(fun g -> g.Key, g.Value.Sum(fun c -> c)).OrderByDescending(fun (g, s) -> s).First()

    guard * Array.findIndex (fun c -> c = schedule.[guard].Max(fun m -> m)) schedule.[guard]

let FindGuardMostFrequentlyAsleepOnMinute(schedule: Map<int, int[]>): int =
    let guard, max = schedule.Select(fun g -> g.Key, g.Value.Max(fun c -> c)).OrderByDescending(fun (g, s) -> s).First()

    guard * Array.findIndex (fun c -> c = schedule.[guard].Max(fun m -> m)) schedule.[guard]

let Solve(): string = 
    let mutable builder = new StringBuilder();
    
    builder <- builder.AppendLine("Day 4: Repose Record");
    builder <- builder.AppendLine()

    let schedule = Parse();

    builder <- builder.AppendLine("Find the guard that has the most minutes asleep. What minute does that guard spend asleep the most?")
    builder <- builder.AppendLine(sprintf "%i" (FindGuardMostMinutesAsleep(schedule)))
    builder <- builder.AppendLine()

    builder <- builder.AppendLine("Of all guards, which guard is most frequently asleep on the same minute?")
    builder <- builder.AppendLine(sprintf "%i" (FindGuardMostFrequentlyAsleepOnMinute(schedule)))
    builder <- builder.AppendLine()

    builder.ToString()