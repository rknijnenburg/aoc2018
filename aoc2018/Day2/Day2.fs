module Day2

open System
open System.IO
open System.Linq

let ReadBoxIDsFromPath(path: string) : string [] =
    File.ReadAllLines(path)

let ReadBoxIDs(): string[] = 
    ReadBoxIDsFromPath("Day2/input.txt")


let CalculateCharacterCount(ID: string) = 
    ID |> Seq.toArray |> Seq.countBy (fun x -> x)


let CalculateBoxIDRecurringValue(ID: string): ( int * int ) =
    let count = ID |> Seq.toArray |> Seq.countBy (fun x -> x)
    (match count.Any(fun x -> snd x = 2) with | true -> 1 | false -> 0) , (match count.Any(fun x -> snd x = 3) with | true -> 1 | false -> 0)


let CalculateChecksum: int =
    let values = ReadBoxIDs() |> Seq.map(fun x -> CalculateBoxIDRecurringValue(x))
    
    values.Sum(fun x -> fst x) * values.Sum(fun x -> snd x)

let CalculateCommonLettersFromIdWithOneCharacterDifference: string =
    let ids = ReadBoxIDs()
    let mutable found = false;
    let mutable result: string = null;

    for id in ids do
        if (not found) then 
            for i = 1 to id.Length - 2 do
                if (not found) then
                    let left = id.Take(i) |> String.Concat
                    let middle = id.[i]
                    let right = id.Skip(i+1) |> String.Concat
                    if (ids.Any(fun x -> x.StartsWith(left) && x.[i] <> middle && x.EndsWith(right))) then
                        result <- left + right
                        found <- true
            
    result

let Solve: string =
    "Day 2\n" +
    sprintf "Waht is the checksum: %i\n" CalculateChecksum +
    sprintf "What letters are common between the two correct box IDs? %s\n" CalculateCommonLettersFromIdWithOneCharacterDifference
