module Day3.NoMatterHowYouSliceIt

open System
open System.IO
open System.Text

type Rectangle = { Id : string option; Left: int; Top: int; Width: int; Height: int; Right: int; Bottom: int }

let BuildRectangle(id: string option, left: int, top: int, width: int, height: int): Rectangle =
    { 
        Id = id; 
        Left = left;
        Top = top;
        Width = width; 
        Height = height;
        Right = left + width - 1
        Bottom = top + height - 1
    }

let ParseRectangle(line: string) : Rectangle =
    let slices = line.Split('@')
    let id = slices[0].Trim()
    let data = slices[1].Split(":");
    let position = data[0].Trim().Split(",")
    let dimensions = data[1].Trim().Split("x")

    BuildRectangle(Some id, Int32.Parse(position[0]), Int32.Parse(position[1]), Int32.Parse(dimensions[0]), Int32.Parse(dimensions[1]))
    
let Parse() = 
    File.ReadAllLines("Day03/input.txt") |> Array.map (fun x -> ParseRectangle(x))

let CalculateCoordinates(rect: Rectangle) : (int * int)[] = 
    [|rect.Left..rect.Right|] |> Array.map (fun x -> [|rect.Top..rect.Bottom|] |> Array.map (fun y -> x, y)) |> Array.concat

let GetIntersectingRectangle(rect1: Rectangle, rect2: Rectangle): Rectangle option =
    let w = Math.Max(0, Math.Min(rect1.Right + 1, rect2.Right + 1) - Math.Max(rect1.Left, rect2.Left));
    let h = Math.Max(0, Math.Min(rect1.Bottom + 1, rect2.Bottom + 1) - Math.Max(rect1.Top, rect2.Top));

    if w > 0 && h > 0 then
      let l = Math.Max(rect1.Left, rect2.Left);
      let t = Math.Max(rect1.Top, rect2.Top)
      Some (BuildRectangle(None, l, t, w, h)) 
    else
        None

let CountIntersections(rectangles: Rectangle[]): int =
    let mutable intersecting : Set<(int * int)> = set []
    
    for i = 0 to rectangles.Length - 2 do
        for j = i + 1 to rectangles.Length - 1 do
            match GetIntersectingRectangle(rectangles.[i], rectangles.[j]) with
            | Some rectangle -> intersecting <- Set.ofArray (CalculateCoordinates(rectangle)) |> Set.union intersecting
            | None -> ()
    
    intersecting.Count;

let FindNotIntersecting(rectangles: Rectangle[]): string =
    let mutable result : string = String.Empty
    let mutable found = false

    for i = 0 to rectangles.Length - 1 do
        if (not found) then
            let mutable intersects = false
            for j = 0 to rectangles.Length - 1 do
                if (i <> j) then
                    match GetIntersectingRectangle(rectangles.[i], rectangles.[j]) with
                        | Some intersection -> intersects <- true
                        | None -> ()                      
            if not intersects then  
                  found <- true
                  result <-rectangles.[i].Id.Value
                    
    result

let Solve(): string = 
    let mutable builder = new StringBuilder();
    
    builder <- builder.AppendLine("Day 3: No Matter How You Slice It");
    builder <- builder.AppendLine()

    let rectangles = Parse();

    builder <- builder.AppendLine("How many square inches of fabric are within two or more claims?")
    builder <- builder.AppendLine(sprintf "%i" (CountIntersections(rectangles)))
    builder <- builder.AppendLine()

    builder <- builder.AppendLine("What is the ID of the only claim that doesn't overlap?")
    builder <- builder.AppendLine(sprintf "%s" (FindNotIntersecting(rectangles)))
    builder <- builder.AppendLine()

    builder.ToString()

