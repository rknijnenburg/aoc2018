module Day13.MineCartMadness

open System.IO
open System.Text

type Direction =
    | Left = 0
    | Up = 1
    | Right = 2
    | Down = 3
type Point = { X: int; Y: int }

type Cart = { mutable X: int; mutable Y: int; mutable Direction: Direction; mutable Intersections: int; mutable Crash: int option}
type Input = { Grid: string[]; mutable Carts: Cart list }

let Parse(): Input =
    let mutable carts: Cart list = []
    let lines = File.ReadAllLines("Day13/input.txt");
    let BuildCart(p: Point, d: Direction): Cart = { X = p.X; Y = p.Y; Direction = d; Intersections = 0; Crash = None }

    for r = 0 to lines.Length - 1 do
        for c = 0 to lines[r].Length - 1 do
            let p = { X = c; Y = r }
            let c = lines[r][c]
            let direction: Direction option = 
                match c with
                | '<' -> Some Direction.Left
                | '^' -> Some Direction.Up
                | '>' -> Some Direction.Right
                | 'v' -> Some Direction.Down
                | _ -> None
            match direction with
            | Some d -> carts <- carts |> List.append [BuildCart(p, d)]
            | None -> ()

    { Grid = lines; Carts = carts; }

let Print(input: Input) =
    for y = 0 to input.Grid.Length - 1 do
        for x = 0 to input.Grid[y].Length - 1 do
            match (input.Carts |> List.tryFind (fun p -> p.X = x && p.Y = y)) with
            | Some cart -> printf "%i" ((int) cart.Direction)
            | None -> printf " "
        printfn ""
    printfn "---"


let UpdateCartPosition(cart: Cart) =
    match (cart.Direction) with
    | Direction.Left -> cart.X <- cart.X - 1
    | Direction.Up -> cart.Y <- cart.Y - 1
    | Direction.Right -> cart.X <- cart.X + 1
    | Direction.Down -> cart.Y <- cart.Y + 1
    | _ -> failwith "Unhandled direction"

let UpdateCartDirection(cart: Cart, c: char) =
    match c with
        | '\\' -> 
            cart.Direction <- 
                match cart.Direction with
                | Direction.Left -> Direction.Up
                | Direction.Up -> Direction.Left
                | Direction.Right -> Direction.Down
                | Direction.Down -> Direction.Right
                | _ -> failwith "Unhandled direction"
        | '/' ->
            cart.Direction <- 
                match cart.Direction with
                | Direction.Left -> Direction.Down
                | Direction.Up -> Direction.Right
                | Direction.Right -> Direction.Up
                | Direction.Down -> Direction.Left
                | _ -> failwith "Unhandled direction"
        | '+' ->
            cart.Direction <-
                match cart.Direction with
                | Direction.Left -> enum<Direction>(((int) Direction.Down + cart.Intersections % 3) % 4)
                | Direction.Up -> enum<Direction>(((int) Direction.Left + cart.Intersections % 3) % 4)
                | Direction.Right -> enum<Direction>(((int) Direction.Up + cart.Intersections % 3) % 4)
                | Direction.Down -> enum<Direction>(((int) Direction.Right + cart.Intersections % 3) % 4)
                | _ -> failwith "Unhandled direction"
            cart.Intersections <- cart.Intersections + 1
        | _ -> ()

let Simulate(): Cart list =
    let input = Parse();
    let grid = input.Grid;
    let mutable carts: Cart list = input.Carts;
    let mutable remaining = carts.Length;
    let mutable crashes = 0;
    
    while (remaining > 1) do
        let ordered = input.Carts |> List.sortBy (fun c -> c.Y, c.X)
        for cart in ordered do
            if (cart.Crash.IsNone) then
                UpdateCartPosition(cart)
                let c = grid[cart.Y][cart.X]
                UpdateCartDirection(cart, c)
                let crashed = 
                    carts 
                    |> List.where (fun c -> c.Crash = None) 
                    |> List.groupBy (fun c -> { X = c.X; Y = c.Y } ) 
                    |> List.where (fun (k, v) -> v.Length > 1) 
                    |> List.map (fun (k, v) -> v) 
                    |> List.tryHead 
                if (crashed <> None) then
                    for cart in crashed.Value do
                        cart.Crash <- Some crashes
                    crashes <- crashes + 1
                    remaining <- (carts |> List.where (fun x -> x.Crash.IsNone)).Length

    carts

//let FindRemainingCart(input: Input): Cart =
//    while (input.Carts.Length > 1) do
//        let crashed = FindCrash(input)
//        input.Carts <- input.Carts |> List.where (fun x -> (crashed |> List.contains x) = false)

//    input.Carts.Head

let Solve(): string =
    let mutable builder = new StringBuilder()

    builder <- builder.AppendLine("Day 13: Mine Cart Madness")
    builder <- builder.AppendLine()

    let carts = Simulate();
    let first = carts |> List.find (fun x -> x.Crash = Some 0)
    let remaining = carts |> List.find (fun x -> x.Crash.IsNone)

    builder <- builder.AppendLine("What is the location of the first crash?")
    builder <- builder.AppendLine(sprintf "%i,%i" first.X first.Y)
        
    builder <- builder.AppendLine("What is the location of the last cart at the end of the first tick where it is the only cart left?")
    builder <- builder.AppendLine(sprintf "%i,%i" remaining.X remaining.Y)
    builder <- builder.AppendLine()

    builder.ToString()