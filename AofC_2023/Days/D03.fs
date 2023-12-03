module D03

open Tools
open Geometry
open System.Text.RegularExpressions

type ValueType = | Number of int | Symbol of char
type Item = { Coordinates: Vector2D[]; Value: ValueType }

//type Part = { Value: int; Coordinates: Vector2D[] }
//type PartOrSymbol = 
//    | Part of Part
//    | Symbol of Vector2D

let part1 input =

    let parseRow index str =
        let matches = Regex.Matches(str, @"((?<digits>\d+)|(?<symbol>[^.]))")
                        |> Seq.toArray
        matches |> Array.map (fun m ->
                                let group = if m.Groups["digits"].Success then m.Groups["digits"] else m.Groups["symbol"]
                                { 
                                    Coordinates = group.Value |> Seq.mapi (fun i f -> { x = i + group.Index; y = index }) |> Seq.toArray
                                    Value = if group.Name = "digits" then ValueType.Number (int group.Value) else ValueType.Symbol group.Value[0]
                                }
                               )

    let all = Parsing.parseRowsIndex input parseRow |> Array.reduce Array.append

    let maxDistance = 1

    let hasSymbolNearby part symbols =
        part.Coordinates |> Array.exists (fun pos -> 
                                   symbols |> Array.exists (fun (xy: Vector2D) -> (pos.sub xy).maxAbs <= maxDistance))

    let isNumber valx = 
        match valx with
        | ValueType.Number s -> true
        | _ -> false

    let parts = all |> Array.filter (fun f -> isNumber f.Value)
    let symbolPositions = all |> Array.filter (fun f -> false = isNumber f.Value) |> Array.map (fun f -> f.Coordinates[0])

    let withNearby = parts |> Array.filter (fun part -> hasSymbolNearby part symbolPositions)

    let result = withNearby |> Array.map (fun f -> match f.Value with | ValueType.Number n -> n | _ -> 0) |> Array.sum
    result
    
let part2 input =
    let parseRow row = [| row |]
    let rows = Parsing.parseRows input parseRow
    let result = 0
    result
