module Day11.ChronalCharge

open System
open System.Text

type Result = { mutable X: int; mutable Y: int; mutable Size: int; mutable Level: int }

let BuildSummedAreaTable(gridSerialNumber: int): int[,] =
    let mutable result = Array2D.create 301 301 0
    for x = 1 to 300 do
        for y = 1 to 300 do
            result.[x, y] <- (((((((((x + 10) * y) + gridSerialNumber) * (x + 10)) / 100) % 10) - 5)) + result.[x, y-1] + result.[x-1, y] - result.[x-1, y-1])

    result

let CalculateMaxSquareLevel(table: int[,], size: int) : Result =
    let mutable result: Result = { X = 0; Y = 0; Size = 0; Level = Int32.MinValue }

    for x = size to 300 do
        for y = size to 300 do
            let level = table.[x, y] - table.[x - size, y] - table.[x, y - size] + table.[x - size, y - size]
            if (level > result.Level) then
                result <- { X = x - size + 1; Y = y - size + 1; Size = size; Level = level }
                
    result
    
let CalculateSquareLargestPower(): Result =
    CalculateMaxSquareLevel(BuildSummedAreaTable(7315), 3)


let CalculateSquareSizeLargestPower(): Result =
    let table = BuildSummedAreaTable(7315)
    let mutable result: Result = { Level = Int32.MinValue; X = 0; Y = 0; Size = 0 }

    for s = 1 to 300 do
        let max = CalculateMaxSquareLevel(table, s)
        if (max.Level > result.Level) then
            result <- max

    result

let Solve(): string = 
    let mutable builder = new StringBuilder()

    builder <- builder.AppendLine("Day 11: Chronal Charge")
    builder <- builder.AppendLine()

    let result = CalculateSquareLargestPower()
    builder <- builder.AppendLine("What is the X,Y coordinate of the top-left fuel cell of the 3x3 square with the largest total power?")
    builder <- builder.AppendLine(sprintf "%i,%i (%i)" result.X result.Y result.Level)
    builder <- builder.AppendLine()

    let result = CalculateSquareSizeLargestPower();
    builder <- builder.AppendLine(sprintf "What is the X,Y,size identifier of the square with the largest total power?" )
    builder <- builder.AppendLine(sprintf "%i,%i,%i (%i)" result.X result.Y result.Size result.Level)
    builder <- builder.AppendLine()

    builder.ToString()

    
    


