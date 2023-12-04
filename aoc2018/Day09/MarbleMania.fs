module Day9.MarbleMania

open System
open System.IO
open System.Text

type Game = { Players: int; mutable Points: int }
type Marble = { Value: int64; mutable Before: Marble option; mutable After: Marble option}

let Parse(): Game =
    File.ReadAllText("Day09/input.txt").Split(" ") |> (fun x -> { Players = Convert.ToInt32(x.[0]); Points = Convert.ToInt32(x.[6]) })        

let Remove(current: Marble): Marble =
    let before = current.Before.Value
    let current = current.After.Value
    current.Before <- Some before
    before.After <- Some current
    current


let Insert(current: Marble, value: int64): Marble =
    let before = current.Before.Value
    let after = current
    let current =  { Value = value; Before = Some before; After = Some after }
    before.After <- Some current
    after.Before <- Some current
    current

let CalculateHighscore(players: int, points: int): int64 = 
    let mutable turn: int = 1
    let mutable current: Marble = { Value = int64 0; Before = None; After = None }
    current.Before <- Some current
    current.After <- Some current
    let mutable scores = Array.init players (fun i -> int64 0)
    
    while (turn < points) do
        if (turn % 23) = 0 then
            for i = 1 to 7 do
                current <- current.Before.Value
            let player = (turn - 1) % players
            Array.set scores player (scores.[player] + int64 turn + current.Value)
            current <- Remove(current)
        else
            for i = 1 to 2 do
                current <- current.After.Value
            current <- Insert(current, int64 turn)

        turn <- turn + 1

    scores |> Seq.max

let Solve(): string =
    let mutable builder = new StringBuilder();
    
    builder <- builder.AppendLine("Day 9: Marble Mania");
    builder <- builder.AppendLine()

    let game = Parse();

    builder <- builder.AppendLine("What is the winning Elf's score?")
    builder <- builder.AppendLine(sprintf "%i" (CalculateHighscore(game.Players, game.Points)))
    builder <- builder.AppendLine()

    builder <- builder.AppendLine("What would the new winning Elf's score be if the number of the last marble were 100 times larger?")
    builder <- builder.AppendLine(sprintf "%i" (CalculateHighscore(game.Players, game.Points * 100)))
    builder <- builder.AppendLine()

    builder.ToString()    