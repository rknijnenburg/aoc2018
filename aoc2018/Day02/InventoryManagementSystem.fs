module Day2.InventoryManagementSystem

open System
open System.IO
open System.Text

let Parse(): string[] = 
    File.ReadAllLines("Day02/input.txt")

let FindRecurringValues(line: string): ( int * int ) =
    let count = line |> Seq.toArray |> Seq.countBy (fun x -> x)
    (
        (match count |> Seq.exists (fun x -> snd x = 2) with | true -> 1 | false -> 0), 
        (match count |> Seq.exists (fun x -> snd x = 3) with | true -> 1 | false -> 0)
    )


let CalculateChecksum(ids: string[]): int =
    let values = ids |> Seq.map(fun x -> FindRecurringValues(x))
    
    (values |> Seq.map (fun x -> fst x) |> Seq.sum) * (values |> Seq.map (fun x -> snd x) |> Seq.sum)

let FindCommonCharacters(ids: string[]): string =
    let mutable found = false;
    let mutable result: string = null;

    for id in ids do
        if (not found) then 
            for i = 1 to id.Length - 2 do
                if (not found) then
                    let left = id |> Seq.take i |> String.Concat
                    let middle = id.[i]
                    let right = id |> Seq.skip (i+1) |> String.Concat
                    if (ids |> Array.exists (fun x -> x.StartsWith(left) && x.[i] <> middle && x.EndsWith(right))) then
                        result <- left + right
                        found <- true
            
    result

let Solve(): string =
    let mutable builder = new StringBuilder();
    
    builder <- builder.AppendLine("Day 2: Inventory Management System");
    builder <- builder.AppendLine()

    let ids = Parse();

    builder <- builder.AppendLine("What is the checksum for your list of box IDs?")
    builder <- builder.AppendLine(sprintf "%i" (CalculateChecksum(ids)))
    builder <- builder.AppendLine()

    builder <- builder.AppendLine("What letters are common between the two correct box IDs?")
    builder <- builder.AppendLine(sprintf "%s" (FindCommonCharacters(ids)))
    builder <- builder.AppendLine()

    builder.ToString()
