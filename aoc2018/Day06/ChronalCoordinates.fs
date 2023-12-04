module Day6.ChronalCoordinates

open System
open System.IO
open System.Text

type Point = { X: int; Y: int }

let Parse(): Set<Point> =
    File.ReadAllLines("Day06/input.txt") 
        |> Seq.map (fun x -> { X = Convert.ToInt32(x.Split(',').[0]); Y = Convert.ToInt32(x.Split(',').[1]) }) 
        |> Set.ofSeq

let GetManhattanDistance(M: Point, P: Point): int = 
    abs(M.X - P.X) + abs(M.Y - P.Y)

let FindLargestArea(coordinates: Set<Point>): int =
    let mutable map: Map<Point, Set<Point>> = coordinates |> Set.map (fun p -> p, Set.ofSeq []) |> Map.ofSeq

    let minX = coordinates |> Set.map (fun c -> c.X) |> Seq.min
    let minY = coordinates |> Set.map (fun c -> c.Y) |> Seq.min
    let maxX = coordinates |> Set.map (fun c -> c.X) |> Seq.max
    let maxY = coordinates |> Set.map (fun c -> c.Y) |> Seq.max

    for x = minX to maxX do
        for y = minY to maxY do
            let mutable sd = Int32.MaxValue
            let mutable sp = None
            for point in coordinates do
                let length = GetManhattanDistance({X = x; Y = y}, point)
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
            |> Map.filter (fun _ value -> value |> Seq.forall (fun p -> p.X > minX && p.Y > minY && p.X < maxX && p.Y < maxY))

    (snd (map |> Map.toSeq |> Seq.sortByDescending (fun x -> (snd x) |> Seq.length) |> Seq.head)).Count
        //.First()
        //.Value
        //.Count


let FindSizeWithDistanceLessThan(coordinates: Set<Point>, maxDistance: int): int =
    let minX = coordinates |> Set.map (fun c -> c.X) |> Seq.min
    let minY = coordinates |> Set.map (fun c -> c.Y) |> Seq.min
    let maxX = coordinates |> Set.map (fun c -> c.X) |> Seq.max
    let maxY = coordinates |> Set.map (fun c -> c.Y) |> Seq.max

    let mutable map: Map<Point, int> = Map.ofSeq []

    for x = minX to maxX do
        for y = minY to maxY do
            let mutable sum = 0
            for point in coordinates do
                sum <- sum + GetManhattanDistance({X = x; Y = y}, point)
            map <- Map.add { X = x; Y = y} sum map
    
    map <- 
        map 
            |> Map.filter (fun _ value -> value < maxDistance)

    map.Count

let Solve(): string = 
    let mutable builder = new StringBuilder();
    
    builder <- builder.AppendLine("Day 6: Chronal Coordinates");
    builder <- builder.AppendLine()

    let coordinates = Parse();

    builder <- builder.AppendLine("What is the size of the largest area that isn't infinite?")
    builder <- builder.AppendLine(sprintf "%i" (FindLargestArea(coordinates)))
    builder <- builder.AppendLine()

    builder <- builder.AppendLine("What is the size of the region containing all locations which have a total distance to all given coordinates of less than 10000?")
    builder <- builder.AppendLine(sprintf "%i" (FindSizeWithDistanceLessThan(coordinates, 10000)))
    builder <- builder.AppendLine()

    builder.ToString()