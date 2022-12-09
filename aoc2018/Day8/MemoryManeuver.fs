module Day8

open System
open System.IO

type Node = { Parent: Node option; mutable Header: int[]; mutable Children : Node list; mutable Metadata : int[]; mutable MetadataValue: int; mutable NodeValue: int}

let ReadLicenseFile(): int[] =
    File.ReadAllText("Day8/input.txt").Split(" ") |> Array.map (fun s -> Convert.ToInt32(s))

let BuildNode(data: int[]): Node =
    let mutable remaining = data
    let mutable current: Node = { Parent = None; Header = remaining.[0..1]; Children = []; Metadata = [||]; MetadataValue = 0; NodeValue = 0 }
    remaining <- remaining.[2..]

    while (remaining.Length > 0) do
        if (current.Children.Length < current.Header.[0]) then
            let parent = current
            current <- { Parent = Some parent; Header = remaining.[0..1]; Children = []; Metadata = [||]; MetadataValue = 0; NodeValue = 0 }
            remaining <- remaining.[2..]
            parent.Children <- List.append parent.Children [current]
        else 
            if (current.Metadata.Length < current.Header.[1]) then
                current.Metadata <- remaining.[0..current.Header.[1]-1]
                current.MetadataValue <- Array.sum current.Metadata
                current.NodeValue <- current.MetadataValue
                remaining <- remaining.[current.Header.[1]..]        
            else
                if (current.Parent.IsSome) then    
                    current <- current.Parent.Value

        if (current.Children.Length = current.Header.[0] && current.Metadata.Length = current.Header.[1]) then
            current.MetadataValue <- Array.sum current.Metadata + (current.Children |> List.map(fun p -> p.MetadataValue) |> List.toArray |> Array.sum)
            if (current.Children.Length = 0) then
                current.NodeValue <- current.MetadataValue
            else
                let mutable sum = 0
                for index in current.Metadata do
                    if (index > 0 && index <= current.Children.Length) then
                        sum <- sum + current.Children.[index - 1].NodeValue
                current.NodeValue <- sum

    current

let CalculateMetadataValue: int = 
    BuildNode(ReadLicenseFile()).MetadataValue

let CalculateNodeValue: int = 
    BuildNode(ReadLicenseFile()).NodeValue

let Solve: string =
    
    sprintf "Day 8\n" +
    sprintf "What is the sum of all metadata entries %i\n" CalculateMetadataValue +
    sprintf "What is the value of the root node? %i\n" CalculateNodeValue