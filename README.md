# AofC_2023

Learning F# by doing [AdventOfCode 2023](https://adventofcode.com/2023)

Previous:
* [2021](https://github.com/JWMB/AofC_2021)
* [2022](https://github.com/JWMB/AofC_2022)


##Autogenerated##
## [Day 1 - Trebuchet?!](https://adventofcode.com/2023/day/1)
[Source](/AofC_2023/Days/D01.fs) | [Input](/AofC_2023/Days/D01.txt)  
### part1
```FSharp
let part1 input =
    let parseRow input =
        let digits = Regex.Replace(input, @"\D", "") |> Seq.toArray
        firstLast digits

    let rowValues = Parsing.parseRows input parseRow
    let result = Seq.sum rowValues
    result
```

Result (in `13`ms): `55607`
### part2
```FSharp
let part2 input =
    let digitWords = "zero one two three four five six seven eight nine".Split ' '
    let digitWordPattern = digitWords |> String.concat "|"
    let pattern = $@"({digitWordPattern}|\d)"

    let makeDigit str =
        match digitWords |> Array.tryFindIndex (fun f -> f = str) with
            | Some s -> s
            | _ -> int str

    let parseRow input =
        let first = Regex.Match(input, pattern)
        let last = Regex.Match(input, pattern, RegexOptions.RightToLeft)
        let matches = [| first; last |]
        let digits = matches |> Array.map (fun f -> makeDigit(f.Value))
        firstLast digits

    let rowValues = Parsing.parseRows input parseRow
    let result = Seq.sum rowValues
    result
```

Result (in `9`ms): `55291`
## [Day 2 - Cube Conundrum](https://adventofcode.com/2023/day/2)
[Source](/AofC_2023/Days/D02.fs) | [Input](/AofC_2023/Days/D02.txt)  
### part1
```FSharp
let part1 input =
    let (=>) x y = x,y
    let makeMap x = new Map<_,_>(x)
    let maxByColor =
        [
            Color.red => 12
            Color.green => 13
            Color.blue => 14
        ] |> makeMap

    let exceedsMax color count =
        count > maxByColor[color]

    let getExceedsMax colorCounts = 
        colorCounts
        |> Map.toArray
        |> Array.filter (fun (color, count) -> exceedsMax color count)

    let anyTurnExceedsMax colorCounts = 
        let a = getExceedsMax colorCounts 
        a |> Array.length > 0

    let rows = Parsing.parseRows input parseRow

    let hasNoMoreThanMax row =
        row.Turns
        |> Array.filter anyTurnExceedsMax
        |> Array.length = 0

    let okIds = rows |> Array.filter hasNoMoreThanMax |> Array.map (fun f -> f.Id)
    let result = okIds |> Array.sum
    result
```

Result (in `20`ms): `2476`
### part2
```FSharp
let part2 input =
    let rows = Parsing.parseRows input parseRow

    let maxOf map1 map2 = 
        merge map1 map2 (fun key (v1: int, v2: int) -> Math.Max(v1, v2))
        
    let minCountsOfRow row =
        row.Turns
        |> Array.reduce (fun p c -> maxOf p c)

    let rowValue row = minCountsOfRow row |> Map.toArray |> Array.map (fun (_, cnt) -> cnt) |> Array.reduce (fun p c -> p * c)

    let rowValues = rows |> Array.map (fun r -> rowValue r)
    let result = rowValues |> Array.sum
    result
```

Result (in `9`ms): `54911`
## [Day 3 - Gear Ratios](https://adventofcode.com/2023/day/3)
[Source](/AofC_2023/Days/D03.fs) | [Input](/AofC_2023/Days/D03.txt)  
### part1
```FSharp
let part1 input =
    let map = constructMap input

    let parts = map.Parts
    let symbolPositions = map.Symbols |> Array.map (fun f -> f.Coordinates[0])

    let maxDistance = 1
    let hasSymbolNearby part symbols =
        part.Coordinates |> Array.exists (fun pos -> 
                                   symbols |> Array.exists (fun (xy: Vector2D) -> (pos.sub xy).maxAbs <= maxDistance))

    let withNearby = parts |> Array.filter (fun part -> hasSymbolNearby part symbolPositions)

    let result = withNearby |> Array.map (fun f -> match f.Value with | ValueType.Number n -> n | _ -> 0) |> Array.sum
    result
```

Result (in `94`ms): `557705`
### part2
```FSharp
let part2 input =
    let map = constructMap input

    let parts = map.Parts

    let gearSymbolCoordinates =
        map.Symbols
        |> Array.filter (fun f -> '*' = match f.Value with | ValueType.Symbol s -> s | _ -> '.')
        |> Array.map (fun f -> f.Coordinates[0])

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
                                |> Array.map (fun p -> match p.Value with | ValueType.Number n -> n | _ -> 1)
                                |> Array.reduce (fun p c -> p * c)
                      )

    let result = products |> Array.sum
    result
```

Result (in `107`ms): `84266818`
