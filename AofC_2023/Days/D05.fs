module D05

open Tools
open System.Text.RegularExpressions
open System

let parseRow row = [| row |]

type Range = { Start: int64; End: int64; } with
    member this.move diff = { Start = this.Start + diff; End = this.End + diff }
    static member optimize (ranges: Range array) =
        let hasOverlap r1 r2 = // 10-20 5-10
            r1.Start <= r2.End && r1.End >= r2.Start ||
            r2.Start <= r1.End && r2.End >= r1.Start
        let merge r1 r2 =
            { Start = Math.Min(r1.Start, r2.Start); End = Math.Max(r1.End, r2.End) }
        let mergeMany (rs: Range array) =
                { Start = rs |> Array.map (fun v -> v.Start) |> Array.min; End = rs |> Array.map (fun v -> v.End) |> Array.max; }

        let tryMerge r1 r2 =
            if hasOverlap r1 r2 then
                Some(merge r1 r2)
            else
                None

        let merges r rest =
            let result = rest |> Array.map (fun f -> (f, tryMerge r f))
            let unmodified = result |> Array.filter (fun (_, opt) -> opt = None) |> Array.map (fun (a, _) -> a)
            let modified = result |> Array.filter (fun (_, opt) -> opt <> None) |> Array.map (fun (_, opt) -> opt.Value)
            //let merged = 
            //    if modified.Length = 0 then [||]
            //    else [| mergeMany modified |]
            {| 
                Merged = if modified.Length = 0 then None else Some(mergeMany modified);
                Unmodified = unmodified
            |}
        let rec loop rs = seq {
            let len = Array.length rs
            if len = 1 then
                yield rs[0]
            else if len > 1 then
                let result = merges rs[0] (rs |> Array.tail)
                if result.Merged.IsNone then yield rs[0]
                let next = result.Unmodified |> Array.append (if result.Merged.IsNone then [||] else [|result.Merged.Value|])
                yield! loop next
        }
        loop ranges |> Seq.toArray
        //let rec loop rs =
        //    let r1 = rs |> Array.head
        //    let r2 = r1
        //    if hasOverlap r1 r2 then
        //        merge r1 r2
        

type Transform = { Src: int64; Dst: int64; Length: int64 } with
    member this.diffSrcToDst = this.Dst - this.Src
    member this.isInRange value = value >= this.Src && value < this.Src + this.Length
    member this.apply value =
        if this.isInRange value then value - this.Src + this.Dst else value
    member this.partition range =
        let rangeEnd = this.Src + this.Length
        if range.Start > rangeEnd || range.End < this.Src then 
            {| Inside = None; Outside = [| { Start = range.Start; End = range.End } |] |}
        else
            let overlap = { Start =Math.Max(this.Src, range.Start); End = Math.Min(rangeEnd, range.End)}
            let below = if range.Start < this.Src then [| { Start = range.Start; End = this.Src - 1L} |] else [||]
            let above = if range.End > rangeEnd then [| { Start = rangeEnd + 1L; End = range.End } |] else [||]
            {| Inside = Some(overlap); Outside = [| above; below |] |> Array.reduce Array.append |}
    member this.applyRange range =
        let partitioned = this.partition range
        let transformed =
            match partitioned.Inside with
            | None -> [||]
            | Some(r) -> [| { Start = r.Start - (this.Src - this.Dst); End = r.End - (this.Src - this.Dst) } |]
        
        let allRanges = partitioned.Outside |> Array.append transformed // |> Array.reduce Array.append
        Range.optimize allRanges

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

    member this.applyRange range =
        let rec loop (transforms: Transform array) ranges = seq {
            if Array.length ranges > 0 then
                if Array.length transforms = 0 then
                    yield ranges
                else
                    let transform = Array.head transforms 
                    let partitions = ranges |> Array.map transform.partition
                    let insides = partitions |> Array.filter (fun f -> f.Inside.IsSome) |> Array.map (fun f -> f.Inside.Value)
                    if Array.length insides > 0 then
                        yield Range.optimize insides |> Array.map (fun f -> f.move transform.diffSrcToDst)
                    //let part = transform.partition range
                    //if part.Inside.IsSome then yield [| part.Inside.Value.move transform.diffSrcToDst |]
                    let outsides = partitions |> Array.map (fun f -> f.Outside) |> Array.reduce Array.append
                    let outsides = Range.optimize outsides
                    yield! loop (Array.tail transforms) outsides
        }

        let result = loop this.Transforms [| range |] |> Seq.toArray |> Array.reduce Array.append
        let result = Range.optimize result
        result

    member this.applyRanges ranges =
        //let allResults = ranges |> Array.map this.applyRange
        let allResults = ranges |> Array.map this.applyRange |> Array.reduce Array.append
        //Range.optimize allResults
        allResults


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
    let sections = parseInput input
    let initialState =
        Regex.Matches(sections[0].Content
        |> String.concat " ", @"\d+")
        |> Seq.toArray |> Array.map (fun f -> int64 f.Value)
        |> Array.chunkBySize 2 
        |> Array.map (fun arr -> { Start = arr[0]; End = arr[0] + arr[1] })

    let parseRange (str: string) =
        let matches = Regex.Matches(str, @"\d+") |> Seq.toArray |> Array.map (fun f -> int64 f.Value)
        { Src = matches[1]; Dst = matches[0]; Length = matches[2] }

    let maps = sections |> Array.tail |> Array.map (fun f -> { Header = f.Header; Transforms = f.Content |> Array.map parseRange })

    //let r = Range.optimize [| { Start = 30; End = 35 }; { Start = 10; End = 20 }; { Start = 5; End = 10 }; { Start = 19; End = 22 } ; { Start = 22; End = 30 } |]
    //let ooo = maps[0].applyRange { Start = 97; End = 102 }
    //let ooo = maps[0].applyRange { Start = 98; End = 99 }
    //let ooo = maps[0].applyRange { Start = 96; End = 97 }
    //let ooo = maps[0].applyRange { Start = 55; End = 68 }

    let applyMaps range (maps: Map array) =
        applyManyPartials [| range |] (maps |> Array.map (fun m -> m.applyRanges)) None |> Seq.toArray |> Array.reduce Array.append


    //let initialState = [| { Start = 82; End = 83 }|]
    let initialState = [| { Start = 79; End = 93 }|]
    let final = initialState |> Array.map (fun range -> applyMaps range maps) //|> Array.reduce Array.append
    let result = final |> Array.map (fun f -> f |> Array.last) |> Array.map (fun r -> r.Start) |> Array.min

    0 //result
