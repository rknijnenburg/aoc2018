﻿module Day6

open System
open System.IO
open System.Linq
open System.Collections.Generic

type Point = { X: int; Y: int }

let ReadCoordinates(): Set<Point> =
    File.ReadAllLines("Day6/input.txt") 
        |> Seq.map (fun x -> { X = Convert.ToInt32(x.Split(',').[0]); Y = Convert.ToInt32(x.Split(',').[1]) }) 
        |> Set.ofSeq

let CalculateManhattanDistance(M: Point, P: Point): int = 
    abs(M.X - P.X) + abs(M.Y - P.Y)

let CalculateLargestSizeArea: int =
    let coordinates = ReadCoordinates()

    let mutable map: Map<Point, Set<Point>> = coordinates |> Set.map (fun p -> p, Set.ofSeq []) |> Map.ofSeq

    let minX = coordinates.Min(fun c -> c.X)
    let minY = coordinates.Min(fun c -> c.Y)
    let maxX = coordinates.Max(fun c -> c.X)
    let maxY = coordinates.Max(fun c -> c.Y)

    for x = minX to maxX do
        for y = minY to maxY do
            let mutable sd = Int32.MaxValue
            let mutable sp = None
            for point in coordinates do
                let length = CalculateManhattanDistance({X = x; Y = y}, point)
                if (length = sd) then
                    sp <- None
                if (length < sd) then
                    sp <- Some point
                    sd <- length
            if (sp.IsSome) then
                map <- map.Add(sp.Value, Set.add { X = x; Y = y } map.[sp.Value])
    
    map <- 
        map 
            |> Map.filter (fun key _ -> key.X > minX && key.Y > minY && key.X < maxX && key.Y < maxY)
            |> Map.filter (fun _ value -> value.All(fun p -> p.X > minX && p.Y > minY && p.X < maxX && p.Y < maxY))

    map
        .OrderByDescending(fun x -> x.Value.Count)
        .First()
        .Value
        .Count


let CalculateSizeWithDistanceLessThan(maxDistance: int): int =
    let coordinates = ReadCoordinates()

    let minX = coordinates.Min(fun c -> c.X)
    let minY = coordinates.Min(fun c -> c.Y)
    let maxX = coordinates.Max(fun c -> c.X)
    let maxY = coordinates.Max(fun c -> c.Y)

    let mutable map: Map<Point, int> = Map.ofSeq []

    for x = minX to maxX do
        for y = minY to maxY do
            let mutable sum = 0
            for point in coordinates do
                sum <- sum + CalculateManhattanDistance({X = x; Y = y}, point)
            map <- Map.add { X = x; Y = y} sum map
    
    map <- 
        map 
            |> Map.filter (fun _ value -> value < maxDistance)

    map.Count

let Solve: string = 
    sprintf "Day 6\n" +
    sprintf "What is the size of the largest area that isn't infinite? %i\n" CalculateLargestSizeArea +
    sprintf "What is the size of the region containing all locations which have a total distance to all given coordinates of less than 10000? %i\n" (CalculateSizeWithDistanceLessThan(10000))