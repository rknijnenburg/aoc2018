module Day9

open System;
open System.IO;

type Game = { Players: int; mutable Points: int }
type Marble = { Value: int64; mutable Before: Marble option; mutable After: Marble option}

let ReadGames(): Game[] =
    File.ReadAllLines("Day9/input.txt") 
        |> Array.map(fun l -> l.Split(" ")) 
        |> Array.map (fun e -> { Players = Convert.ToInt32(e.[0]); Points = Convert.ToInt32(e.[6])})

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

let CalculateHighscoreFromGame(game): int64 = 
    let mutable turn: int = 1
    let mutable current: Marble = { Value = int64 0; Before = None; After = None }
    current.Before <- Some current
    current.After <- Some current
    let mutable scores = Array.init game.Players (fun i -> int64 0)
    
    while (turn < game.Points) do
        if (turn % 23) = 0 then
            for i = 1 to 7 do
                current <- current.Before.Value
            let player = (turn - 1) % game.Players
            Array.set scores player (scores.[player] + int64 turn + current.Value)
            current <- Remove(current)
        else
            for i = 1 to 2 do
                current <- current.After.Value
            current <- Insert(current, int64 turn)

        turn <- turn + 1

    scores
        |> Set.ofArray
        |> Set.maxElement

let CalculateHighscore: int64 =
    CalculateHighscoreFromGame(ReadGames().[0])

let CalculateHighscoreTimesHundred: int64 =
    let game = ReadGames().[0]

    game.Points <- game.Points * 100

    CalculateHighscoreFromGame(game)

let Solve: string =
    sprintf "Day 9\n" +
    sprintf "What is the winning Elf's score? %i\n" CalculateHighscore +
    sprintf "What would the new winning Elf's score be if the number of the last marble were 100 times larger? %i\n" CalculateHighscoreTimesHundred
    