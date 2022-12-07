module Day3

open System
open System.IO

type Rectangle = { ID : string option ; Left: int ; Top: int ; Width: int ; Height: int }

let Right(rect: Rectangle): int = 
    rect.Left + rect.Width - 1
    
let Bottom(rect: Rectangle): int = 
    rect.Top + rect.Height - 1

let ParseOffset(text: string) : (int * int) =
    let split = text.Replace(":", "").Split(',') 
    Int32.Parse(split.[0]) , Int32.Parse(split.[1])

let ParseSize(text: string) : (int * int) =
    let split = text.Split('x') 
    Int32.Parse(split.[0]) , Int32.Parse(split.[1])

let ParseRectangle(text: string) : Rectangle =
    let split = text.Split(' ')
    let left , top = ParseOffset(split.[2])
    let width , height = ParseSize(split.[3])
    
    { ID = Some split.[0] ; Left = left ; Top = top ; Width = width; Height = height }

let ReadRectangles() = 
    File.ReadAllLines("Day3/input.txt") |> Array.map (fun x -> ParseRectangle(x))

let CalculateCoordinates(rect: Rectangle) : (int * int)[] = 
    [|rect.Left..Right(rect)|] |> Array.map (fun x -> [|rect.Top..Bottom(rect)|] |> Array.map (fun y -> x, y)) |> Array.concat

let CalculateIntersectingRectangle(rect1: Rectangle, rect2: Rectangle): Rectangle option =
    let w = Math.Max(0, Math.Min(Right(rect1) + 1, Right(rect2) + 1) - Math.Max(rect1.Left, rect2.Left));
    let h = Math.Max(0, Math.Min(Bottom(rect1) + 1, Bottom(rect2) + 1) - Math.Max(rect1.Top, rect2.Top));

    if w > 0 && h > 0 then
      Some { ID = None ; Left = Math.Max(rect1.Left, rect2.Left) ; Top = Math.Max(rect1.Top, rect2.Top) ; Width = w; Height = h }
    else
        None

let CalculateIntersections: int =
    let rectangles = ReadRectangles()
    let mutable intersecting : Set<( int * int )> = set []
    
    for i = 0 to rectangles.Length - 2 do
        for j = i + 1 to rectangles.Length - 1 do
            match CalculateIntersectingRectangle(rectangles.[i], rectangles.[j]) with
            | Some rectangle -> 
                for coordinate in CalculateCoordinates(rectangle) do
                    intersecting <- intersecting.Add(coordinate)
            | None -> ()
            
    
    intersecting.Count;

let CalculateNonIntersectingId: string =
    let rectangles = ReadRectangles()
    let mutable result : string = String.Empty
    let mutable found = false

    for i = 0 to rectangles.Length - 1 do
        if (not found) then
            let mutable intersects = false
            for j = 0 to rectangles.Length - 1 do
                if (i <> j) then
                    match CalculateIntersectingRectangle(rectangles.[i], rectangles.[j]) with
                        | Some intersection -> intersects <- true
                        | None -> ()                      
            if not intersects then  
                  found <- true
                  result <-rectangles.[i].ID.Value
                    
    result

let Solve: string = 
    sprintf "Day 3\n" +
    sprintf "How many square inches of fabric are within two or more claims?: %i\n" CalculateIntersections +
    sprintf "What is the ID of the only claim that doesn't overlap?: %s\n" CalculateNonIntersectingId 

