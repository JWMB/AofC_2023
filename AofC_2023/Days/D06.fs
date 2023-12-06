module D06

open Tools
open System.Text.RegularExpressions
open System


let numWaysToBeatRecord maxTime recordDistance =
    let quadraticSolutions a b c =
        let forSqrt = (b * b) - (4.0 * a * c)
        if forSqrt < 0 then
            None
        else
            let sqrt = System.Math.Sqrt(forSqrt)
            Some((
                    (-b + sqrt) / 2.0 * a,
                    (-b - sqrt) / 2.0 * a
            ))

    //(maxTime - pressTime) * pressTime = distance
    //ax^2 + bx + c = 0
    //-1*presstime^2 + maxTime*presstime + -1*distance = 0 
    let a = -1
    let b = maxTime
    let c = -recordDistance
    let sols = quadraticSolutions a b c
    match sols with
        | None -> 0
        | Some(v) ->
            let isInt = (fst v) = Math.Round(fst v)
            let min = Math.Max(0.0, fst v)
            let max = Math.Min(1.0 *maxTime, snd v)
            let diff = (Math.Floor(max)) - (Math.Ceiling min)
            1 + int diff - (if isInt then 2 else 0)

type Race = { Time: double; Distance: double }
let part1 input =
    let parseRow (row: string) =
        let keyValues = row.Split ':'
        let values = Regex.Matches(keyValues[1], @"\d+") |> Seq.toArray |> Array.map(fun f -> double f.Value)
        (keyValues[0], values)

    let rows = Parsing.parseRows input parseRow |> Map.ofArray
    let races = rows["Time"] |> Array.mapi (fun i v -> { Time = v; Distance = rows["Distance"][i]})

    let partial = races |> Array.map (fun race -> numWaysToBeatRecord race.Time race.Distance)
    let result = partial |> Array.reduce (fun p c -> p * c)
    result
    
let part2 input =
    let parseRow (row: string) =
        let keyValues = row.Split ':'
        let values = double (keyValues[1].Replace(" ", ""))
        (keyValues[0], values)

    let rows = Parsing.parseRows input parseRow |> Map.ofArray
    let race = { Time = rows["Time"]; Distance = rows["Distance"]}
    let result = numWaysToBeatRecord race.Time race.Distance

    result
