module D10

open Tools
open Tools.Geometry
open System.Text.RegularExpressions

let symbolInput = """
    | is a vertical pipe connecting north and south.
    - is a horizontal pipe connecting east and west.
    L is a 90-degree bend connecting north and east.
    J is a 90-degree bend connecting north and west.
    7 is a 90-degree bend connecting south and west.
    F is a 90-degree bend connecting south and east.
    . is ground; there is no pipe in this tile.
    S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.
"""

type Connector = { Connections: Vector2D array }

let directionToVector dir =
    match dir with
    | 0 -> { x = 0; y = -1; }
    | 1 -> { x = 1; y = 0; }
    | 2 -> { x = 0; y = 1; }
    | _ -> { x = -1; y = 0; }

let symbols =
    let parseSymbolRow (row: string) = 
        let row = row.Trim()
        let symbol = row[0];
        let directions = [| "north"; "east"; "south"; "west"|]
        let dirsAlt = directions |> String.concat "|"
        let patternDirs = $"({dirsAlt})";
        let matches = Regex.Matches(row, patternDirs) |> Seq.toArray |> Array.map (fun f -> f.Value)
        if matches.Length = 2 then
            {| Symbol = symbol; Connector = {
                Connections = matches |> Array.map (fun f -> directions |> Array.findIndex (fun v -> v = f)) |> Array.map (fun f -> directionToVector f)
            } |}
        else
            {| Symbol = symbol; Connector = { Connections = [||] } |}
    Parsing.parseRows symbolInput parseSymbolRow

let hsl2rgb (h:float32) (s:float32) (l:float32) =
    //let hsl = new SixLabors.ImageSharp.ColorSpaces.Hsl(0f, 0f, 0f)
    //let c = new SixLabors.ImageSharp.ColorSpaces.Conversion.ColorSpaceConverter().ToRgb(hsl);
    let hueToRgb (p: float32) (q: float32) (t: float32) =
        let t = t +
                    if t < 0f then 1f
                    else if t > 1f then -1f
                    else 0f

        if t < 0.16666666666666666666666666666667f then
            p + (q - p) * 6f * t
        else if t < 0.5f then
            q
        else if t < 0.66666666666666666666666666666667f then 
            p + (q - p) * (0.66666666666666666666666666666667f - t) * 6f
        else p

    if s = 0f then
        (l, l, l) // achromatic
    else
        let q = if l < 0.5f then l * (1f + s) else l + s - l * s
        let p = 2f * l - q
        (
            hueToRgb p q (h + 0.33333333333333333333333333333333f),
            hueToRgb p q h,
            hueToRgb p q (h - 0.33333333333333333333333333333333f)
        )

let createAnimation path size clrSeq =
    let rec loop (img: SixLabors.ImageSharp.Image<SixLabors.ImageSharp.PixelFormats.Rgba32>) (pixelChunks: (SixLabors.ImageSharp.Color * Vector2D) array seq) = seq {
        if false = Seq.isEmpty pixelChunks then
            let pixels = pixelChunks |> Seq.head
            let image = img.Clone()
            for (color, coord) in pixels do
                image[coord.x, coord.y] <- color
            yield image
            yield! loop image (pixelChunks |> Seq.tail)
    }

    let startImage = new SixLabors.ImageSharp.Image<SixLabors.ImageSharp.PixelFormats.Rgba32>(size.x, size.y, SixLabors.ImageSharp.Color.Black)
    let animSeq = loop startImage (clrSeq |> Seq.chunkBySize 100)

    Gif.saveAsGif path (Gif.createGif 1 animSeq)

let part1 input =
    let ss = symbols |> Array.map (fun f -> (f.Symbol, f.Connector)) |> Map.ofArray
    let parseRow row =
        let getSymbol c = 
            let c = if ss.ContainsKey(c) then c else '.'
            ss[c]
        row |> Seq.map (fun c -> getSymbol c) |> Seq.toArray

    let rows = Parsing.parseRows input (fun row -> row |> Seq.toArray)

    let findFirstCoordinate (char: char) =
        let found = rows |> Array.mapi (fun y r -> 
                                             let index = r |> Array.tryFindIndex ((=) char)
                                             match index with
                                             | None -> None
                                             | Some(x) -> Some({ x = x; y = y})
                                       ) |> Array.filter (fun f -> f.IsSome)
        if found.Length = 0 then None
        else found[0]


    let start = findFirstCoordinate 'S'
    if start.IsNone then failwith "Start not found"
    let grid = Parsing.parseRows input parseRow

    let costGrid = grid |> Array.map (fun row -> row |> Array.map (fun _ -> -1))

    //let rec loop (coord: Vector2D) (visited: Vector2D array) =
    //    let coordinatePreviousCost = costGrid[coord.y][coord.x]
    //    let continueLoop = coordinatePreviousCost < 0 || coordinatePreviousCost > visited.Length

    //    if continueLoop then
    //        costGrid[coord.y][coord.x] <- visited.Length

    //        let cell = grid[coord.y][coord.x]
    //        let notVisited = cell.Connections |> Array.except visited
    //        let visited = [| coord |] |> Array.append visited
    //        if notVisited.Length > 0 then
    //            for diff in notVisited do
    //                loop (coord.add diff) visited

    let coloringSequence = new System.Collections.Generic.List<(SixLabors.ImageSharp.Color * Vector2D)>();
    let precalcGrid = grid |> Array.mapi (fun y row -> row |> Array.mapi (fun x v -> v.Connections |> Array.map (fun c -> c.add {x = x; y = y})))
    let walkPipe start lastCoord =
        let mutable continueLooping = true
        let mutable coord = start
        let mutable last = lastCoord
        let mutable numSteps = 0
        while continueLooping do
            let cell = precalcGrid[coord.y][coord.x]
            let next = cell |> Array.except [| last |]
            let cost = costGrid[coord.y][coord.x]
            numSteps <- numSteps + 1
            if cost < 0 || cost > numSteps then
                costGrid[coord.y][coord.x] <- numSteps
                let hLength = 2000
                let hue = (float32 (numSteps % hLength)) / (float32 hLength)
                let lLength = 50000
                let l = (float32 (numSteps % lLength)) / (float32 lLength)
                let (r, g, b) = hsl2rgb hue 0.5f (0.3f + 0.6f*l)
                let to255 fl = byte (255f * fl)
                let color = new SixLabors.ImageSharp.Color(new SixLabors.ImageSharp.PixelFormats.Rgba32(to255 r, to255 g, to255 b)) //255uy, 0uy, (byte)(numSteps % 256)))
                coloringSequence.Add (color, coord)
                continueLooping <- next.Length > 0 && next[0] <> start
                if continueLooping then
                    last <- coord
                    coord <- next[0]
            else
                continueLooping <- false

    let debugGrid gridWithCosts =
        gridWithCosts |> Array.map (fun row -> row |> Array.map (fun v -> if v < 0 then ' ' else 'x') |> System.String) |> String.concat "\n"
    

    let actualStarts =
        [|0..3|]
        |> Array.map directionToVector
        |> Array.map start.Value.add 
        |> Array.filter (fun f -> f.x >= 0 && f.y >= 0)
        |> Array.filter (fun f -> precalcGrid[f.y][f.x] |> Array.contains start.Value)

    costGrid[start.Value.y][start.Value.x] <- 0
    for actualStart in actualStarts do
        walkPipe actualStart start.Value

    let path = "Days/D10part1.gif"
    if false = System.IO.File.Exists path then
        createAnimation path { x = grid[0].Length; y = grid.Length } (coloringSequence |> List.ofSeq)

    let result = costGrid |> Array.reduce Array.append |> Array.filter (fun f -> f >= 0) |> Array.max
    result
    
let part2 input =
    let parseRow row = [| row |]
    let rows = Parsing.parseRows input parseRow
    let result = 0
    result
