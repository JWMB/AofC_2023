module D09

open Tools
open System.Text.RegularExpressions

let parseRow (row: string) = Regex.Matches(row, @"-?\d+") |> Seq.toArray |> Array.map (fun f -> int64 f.Value)

let reduce (arr: int64 array) =
    arr |> Array.pairwise |> Array.map (fun (a, b) -> b - a)

let getValueFromEachStep arr (funcGetYield: int64 array -> int64) =
    let rec untilZero a step = seq {
        yield funcGetYield a
        let valuesLeft = a |> Array.distinct
        if valuesLeft.Length > 1 then
            let reduced = reduce a
            yield! untilZero reduced (step + 1)
        else if valuesLeft[0] <> 0 then
            yield 0
    }
    untilZero arr 0 |> Seq.toArray

let part1 input =
    let rows = Parsing.parseRows input parseRow
    let lastValues = rows |> Array.map (fun f -> getValueFromEachStep f Array.last)
    let newLastValues = lastValues |> Array.map (fun a -> a |> Array.sum)

    let result = newLastValues |> Array.sum
    result
    
let part2 input =
    let rows = Parsing.parseRows input parseRow
    let firstValues = rows |> Array.map (fun f -> getValueFromEachStep f (fun a -> a[0]))
    let newFirstValues = firstValues  |> Array.map (fun arr -> arr |> Array.rev |> Array.reduce (fun a b -> b - a))

    let result = newFirstValues |> Array.sum
    result