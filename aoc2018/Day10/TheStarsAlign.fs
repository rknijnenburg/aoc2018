module Day10.TheStarsAlign

open System
open System.IO
open System.Text.RegularExpressions
open System.Text

type Light = { mutable X: int; mutable Y: int; VX: int; VY: int }
type Box = { MinX: int; MaxX: int; MinY: int; MaxY: int }

let Parse(): Set<Light> =
    let mutable result: Set<Light> = Set.ofSeq []

    let matches = Regex.Matches(File.ReadAllText("Day10/input.txt"), "position=<(?<x>.*),(?<y>.*)> velocity=<(?<vx>.*),(?<vy>.*)>")

    for m in matches do
        if m.Success then
            result <- result.Add({ X = Convert.ToInt32(m.Groups.["x"].Value); Y = Convert.ToInt32(m.Groups.["y"].Value); VX = Convert.ToInt32(m.Groups.["vx"].Value); VY = Convert.ToInt32(m.Groups.["vy"].Value) })

    result

let ReplaceStringAt(text: string, index: int, replacement: string) : string =
    text.Substring(0, index) + replacement + text.Substring(index + replacement.Length)

let Output(lights: Set<Light>): char[,] =
    let minX = lights |> Set.map (fun l -> l.X) |> Set.minElement
    let minY = lights |> Set.map (fun l -> l.Y) |> Set.minElement
    let maxX = lights |> Set.map (fun l -> l.X) |> Set.maxElement
    let maxY = lights |> Set.map (fun l -> l.Y) |> Set.maxElement

    let output = Array2D.create (maxY - minY + 1) (maxX - minX + 1) '.'

    for light in lights do
        let y = light.Y - minY
        let x = light.X - minX
        output.[y,x] <- '#'

    output

let GetNextHeight(lights: Set<Light>): int =
    let set = lights |> Set.map (fun l -> l.Y + l.VY)
    (set |> Set.maxElement) - (set |> Set.minElement)

let FindMessage(lights: Set<Light>): char[,] * int = 
    let mutable duration = 0
    let mutable ph = Int32.MaxValue
    let mutable nh = GetNextHeight(lights)

    while (ph > nh) do
        duration <- duration + 1
        for light in lights do
            light.X <- light.X + light.VX
            light.Y <- light.Y + light.VY
        ph <- nh
        nh <- GetNextHeight(lights)
        
    let text = Output(lights)    

    text, duration


let Solve(): string =
    let mutable builder = new StringBuilder();

    builder <- builder.AppendLine("Day 10: The Stars Align");
    builder <- builder.AppendLine()

    let lights = Parse();

    builder <- builder.AppendLine("What message will eventually appear in the sky?");
    let stars, seconds = FindMessage(lights)
    for y = 0 to (Array2D.length1 stars) - 1 do
        for x = 0 to (Array2D.length2 stars) - 1 do
             builder <- builder.Append(sprintf "%c" stars.[y, x]);
        builder <- builder.AppendLine()
    builder <- builder.AppendLine()

    builder <- builder.AppendLine("exactly how many seconds would they have needed to wait for that message to appear?")
    builder <- builder.AppendLine(sprintf "%i" seconds)
    builder <- builder.AppendLine()


    builder.ToString()