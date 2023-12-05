module D05

open Tools
open System.Text.RegularExpressions

let parseRow row = [| row |]

type Transform = { Src: int64; Dst: int64; Length: int64 } with
    member this.apply value =
        let isInRange = value >= this.Src && value < this.Src + this.Length
        if isInRange then value - this.Src + this.Dst else value
    member this.partition start length =
        let under = start - this.Src
        let within = 0
        let above = 0
        {| Inside = (0, 1); Outside = [| (0, 1); (0, 1) |] |}

//let applyMany value functions exitPredicate =
//    let rec loop value funcs =
//        let transformed = (funcs |> Array.head) value
//        let exit = 
//            match exitPredicate with
//            | Some(f) -> f value transformed
//            | None -> false

//        match exit with
//            | true -> transformed
//            | false ->
//                match funcs.Length with
//                    | 1 -> transformed
//                    | _ -> loop transformed (funcs |> Array.tail)
//    loop value functions

let applyManyPartials value functions exitPredicate =
    let rec loop value funcs =
        seq {
            let transformed = (funcs |> Array.head) value
            let exit = 
                match exitPredicate with
                | Some(f) -> f value transformed
                | None -> false

            match exit with
                | true -> yield transformed
                | false ->
                    match funcs.Length with
                        | 1 -> yield transformed
                        | _ -> yield transformed
                               yield! loop transformed (funcs |> Array.tail)
        }
    loop value functions

type Map = { Header: string; Transforms: Transform array } with
    member this.apply value =
        let exitFunc org modified = org <> modified
        applyManyPartials value (this.Transforms |> Array.map(fun t -> t.apply)) (Some(exitFunc)) |> Seq.last
    member this.appy start length =
        

let parseInput (input: string) =
    let input = input.Replace("\r", "\n")
    let sections = Regex.Matches(input, @"[^\n][-\w ]+:")
                |> Seq.toArray
                |> Array.map (fun f -> {| Index = f.Index; Length = f.Length; Value = f.Value |})

    let sections = 
        [| {| Index = input.Length; Length = 0; Value = "" |} |] |> Array.append sections
        |> Array.pairwise
        |> Array.map (fun (f, s) ->  {|
            Header = f.Value;
            Content = input[f.Index + f.Length..s.Index-1].Trim().Split('\n') |> Array.filter (fun f-> f.Length > 0)
        |})

    sections

let part1 (input: string) =
    let sections = parseInput input
    let initialState = Regex.Matches(sections[0].Content |> String.concat " ", @"\d+") |> Seq.toArray |> Array.map (fun f -> int64 f.Value)

    let parseRange (str: string) =
        let matches = Regex.Matches(str, @"\d+") |> Seq.toArray |> Array.map (fun f -> int64 f.Value)
        { Src = matches[1]; Dst = matches[0]; Length = matches[2] }

    let maps = sections |> Array.tail |> Array.map (fun f -> { Header = f.Header; Transforms = f.Content |> Array.map parseRange })

    let applyMaps value (maps: Map array) =
        applyManyPartials value (maps |> Array.map (fun m -> m.apply)) None |> Seq.toArray

    let final = initialState |> Array.map (fun value -> applyMaps value maps)
    let result = final |> Array.map (fun f -> f |> Array.last) |> Array.min
    result
    
let part2 input =
    0

let part2x input =
    let sections = parseInput input
    let initialState = Regex.Matches(sections[0].Content |> String.concat " ", @"\d+") |> Seq.toArray |> Array.map (fun f -> int64 f.Value)
    //let initialState = initialState |> Array.chunkBySize 2 |> Array.map (fun arr -> [| arr[0]..arr[1]+arr[0]|]) |> Array.reduce Array.append

    let parseRange (str: string) =
        let matches = Regex.Matches(str, @"\d+") |> Seq.toArray |> Array.map (fun f -> int64 f.Value)
        { Src = matches[1]; Dst = matches[0]; Length = matches[2] }

    let maps = sections |> Array.tail |> Array.map (fun f -> { Header = f.Header; Transforms = f.Content |> Array.map parseRange })

    let applyMaps value (maps: Map array) =
        applyManyPartials value (maps |> Array.map (fun m -> m.apply)) None |> Seq.toArray

    let final = initialState |> Array.map (fun value -> applyMaps value maps)
    let result = final |> Array.map (fun f -> f |> Array.last) |> Array.min
    result