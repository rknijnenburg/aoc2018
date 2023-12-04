module Day8.MemoryManeuver

open System
open System.IO
open System.Text

type Node = { Parent: Node option; mutable Header: int[]; mutable Children : Node list; mutable Metadata : int[]; mutable Sum: int; mutable Value: int}

let Parse(): Node =
    let mutable remaining = File.ReadAllText("Day08/input.txt").Split(" ") |> Array.map (fun s -> Convert.ToInt32(s))
    let mutable current: Node = { Parent = None; Header = remaining.[0..1]; Children = []; Metadata = [||]; Sum = 0; Value = 0 }
    remaining <- remaining.[2..]

    while (remaining.Length > 0) do
        if (current.Children.Length < current.Header.[0]) then
            let parent = current
            current <- { Parent = Some parent; Header = remaining.[0..1]; Children = []; Metadata = [||]; Sum = 0; Value = 0 }
            remaining <- remaining.[2..]
            parent.Children <- List.append parent.Children [current]
        else 
            if (current.Metadata.Length < current.Header.[1]) then
                current.Metadata <- remaining.[0..current.Header.[1]-1]
                current.Sum <- Array.sum current.Metadata
                current.Value <- current.Sum
                remaining <- remaining.[current.Header.[1]..]        
            else
                if (current.Parent.IsSome) then    
                    current <- current.Parent.Value

        if (current.Children.Length = current.Header.[0] && current.Metadata.Length = current.Header.[1]) then
            current.Sum <- Array.sum current.Metadata + (current.Children |> List.map(fun p -> p.Sum) |> List.toArray |> Array.sum)
            if (current.Children.Length = 0) then
                current.Value <- current.Sum
            else
                let mutable sum = 0
                for index in current.Metadata do
                    if (index > 0 && index <= current.Children.Length) then
                        sum <- sum + current.Children.[index - 1].Value
                current.Value <- sum

    current

let Solve(): string =
    let mutable builder = new StringBuilder();
    
    builder <- builder.AppendLine("Day 8: Memory Maneuver");
    builder <- builder.AppendLine()

    let node = Parse();

    builder <- builder.AppendLine("What is the sum of all metadata entries?")
    builder <- builder.AppendLine(sprintf "%i" (node.Sum))
    builder <- builder.AppendLine()

    builder <- builder.AppendLine("What is the value of the root node?")
    builder <- builder.AppendLine(sprintf "%i" (node.Value))
    builder <- builder.AppendLine()

    builder.ToString()