module D03

open Tools
open Geometry
open System.Text.RegularExpressions

type Part = { Value: int; Coordinates: Vector2D[] }
type Symbol = { Value: char; Coordinate: Vector2D }
type PartOrSymbol = 
    | Part of Part
    | Symbol of Symbol

let constructMap input =
    let groupNameDigits = "digits"
    let groupNameSymbol = "symbol"
    let parseRow index str =
        let matches = Regex.Matches(str, @$"((?<{groupNameDigits}>\d+)|(?<{groupNameSymbol}>[^.]))")
                        |> Seq.toArray
        matches |> Array.map (fun m ->
                                let group = if m.Groups[groupNameDigits].Success then m.Groups[groupNameDigits] else m.Groups[groupNameSymbol]
                                if group.Name = groupNameDigits then
                                    PartOrSymbol.Part { Value = int group.Value; Coordinates = group.Value |> Seq.mapi (fun i _ -> { x = i + group.Index; y = index }) |> Seq.toArray }
                                else
                                    PartOrSymbol.Symbol { Value = group.Value[0]; Coordinate = { x = group.Index; y = index } }
                               )

    let all = Parsing.parseRowsIndex input parseRow |> Array.reduce Array.append

    {|
        Parts = all |> Array.choose (fun f -> match f with | PartOrSymbol.Part x -> Some(x) | _ -> None)
        Symbols = all |> Array.choose (fun f -> match f with | PartOrSymbol.Symbol x -> Some(x) | _ -> None)
    |}

let part1 input =
    let map = constructMap input

    let symbolPositions = map.Symbols |> Array.map (fun f -> f.Coordinate)

    let maxDistance = 1
    let hasSymbolNeaby (coord: Vector2D) = 
        symbolPositions |> Array.exists (fun (xy: Vector2D) -> (coord.sub xy).maxAbs <= maxDistance)

    let hasSymbolNearby part =
        part.Coordinates |> Array.exists hasSymbolNeaby

    let partsWithSymbolNearby = map.Parts |> Array.filter (fun part -> hasSymbolNearby part)

    let result = partsWithSymbolNearby |> Array.sumBy (fun f -> f.Value)
    result
    
let part2 input =
    let map = constructMap input

    let parts = map.Parts

    let gearSymbolCoordinates =
        map.Symbols
        |> Array.filter (fun f -> f.Value = '*')
        |> Array.map (fun f -> f.Coordinate)

    let maxDistance = 1

    let partsNearby coord =
        parts
        |> Array.filter (fun p ->
                        p.Coordinates
                        |> Array.exists (fun pos -> (pos.sub coord).maxAbs <= maxDistance)
                        )
        
    let gearsWithParts =
        gearSymbolCoordinates 
        |> Array.map (fun f -> {| Loc = f; Parts = partsNearby f; |})
        |> Array.filter (fun f -> f.Parts.Length = 2)

    let products =
        gearsWithParts
        |> Array.map (fun f -> f.Parts
                                |> Array.map (fun p -> p.Value)
                                |> Array.reduce (fun p c -> p * c)
                      )

    let result = products |> Array.sum
    result
