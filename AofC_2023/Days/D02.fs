module D02

open Tools
open System

type Color =
    | red = 0
    | green = 1
    | blue = 2

// Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
type XX = Map<Color, int>
type Game = { Id: int; Turns: Map<Color, int> array }

let getColor (str: String) = Enum.Parse(typedefof<Color>, str) :?> Color

let parseRow (row: string) = 
    let gameIdAndData = row.Split(':')
    let data =
        gameIdAndData[1].Trim().Split(';')
        |> Array.map (fun turns -> 
            turns.Trim().Split(',') 
                |> Array.map (fun colorAndCount -> 
                    let kv = colorAndCount.Trim().Split(' ')
                    ( getColor kv[1], int kv[0] )
                    )
                |> Map
             )
                
    { Id = int (gameIdAndData[0].Split(' ')[1]); Turns = data }

let merge (a : Map<'a, 'b>) (b : Map<'a, 'b>) (f : 'a -> 'b * 'b -> 'b) =
    Map.fold (fun s k v ->
        match Map.tryFind k s with
        | Some v' -> Map.add k (f k (v, v')) s
        | None -> Map.add k v s) a b

let part1 input =
    let (=>) x y = x,y
    let makeMap x = new Map<_,_>(x)
    let maxByColor =
        [
            Color.red => 12
            Color.green => 13
            Color.blue => 14
        ] |> makeMap

    let getExceedsMax colorCounts = 
        colorCounts
        |> Map.toArray
        |> Array.filter (fun (color, count) -> count > maxByColor[color])

    let anyTurnExceedsMax colorCounts = 
        getExceedsMax colorCounts 
        |> Array.length > 0

    let rows = Parsing.parseRows input parseRow

    let hasNoMoreThanMax row =
        row.Turns
        |> Array.filter anyTurnExceedsMax
        |> Array.length = 0

    let okIds = rows |> Array.filter hasNoMoreThanMax |> Array.map (fun f -> f.Id)
    let result = okIds |> Array.sum
    result
    
let part2 input =
    let rows = Parsing.parseRows input parseRow

    let maxOf map1 map2 = 
        merge map1 map2 (fun _ (v1: int, v2: int) -> Math.Max(v1, v2))
        
    let minCountsOfRow row =
        row.Turns
        |> Array.reduce (fun p c -> maxOf p c)

    let rowValue row =
        minCountsOfRow row
        |> Map.toArray
        |> Array.map (fun (_, cnt) -> cnt)
        |> Array.reduce (fun p c -> p * c)

    let rowValues = rows |> Array.map (fun r -> rowValue r)
    let result = rowValues |> Array.sum
    result
