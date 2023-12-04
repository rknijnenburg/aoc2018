// Learn more about F# at http://fsharp.org

open System
open System.Diagnostics

let execute(f: Func<string>) =
    let stopwatch = Stopwatch.StartNew();

    printfn "---------------------------------------------------------"
    printf "%s" (f.Invoke())
    printfn "elapsed: %s" (stopwatch.Elapsed.ToString())
    printfn ""


[<EntryPoint>]
let main argv =

    execute(Day1.ChronalCalibration.Solve);
    execute(Day2.InventoryManagementSystem.Solve);
    execute(Day3.NoMatterHowYouSliceIt.Solve);
    execute(Day4.ReposeRecord.Solve);
    execute(Day5.AlchemicalReduction.Solve);
    execute(Day6.ChronalCoordinates.Solve);
    execute(Day7.TheSumOfItsParts.Solve);
    execute(Day8.MemoryManeuver.Solve);
    execute(Day9.MarbleMania.Solve);
    execute(Day10.TheStarsAlign.Solve);
    execute(Day11.ChronalCharge.Solve);
    execute(Day12.SubterraneanSustainability.Solve);
    execute(Day13.MineCartMadness.Solve);

    Console.ReadKey() |> ignore

    0 // return an integer exit code

