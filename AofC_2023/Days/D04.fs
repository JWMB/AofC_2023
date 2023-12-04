module D04

open Tools

let parseRow (row: string) =
    let row = row.Split(':')[1];
    row.Split('|')
        |> Array.map (fun f -> 
                        f.Trim().Split(' ')
                        |> Array.map (fun v -> v.Trim())
                        |> Array.filter (fun v -> v.Length > 0)
                        |> Array.map int)

let part1 input =
//Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    let rows = Parsing.parseRows input parseRow

    let cardCorrectValues (row: int array array) =
        Set.intersect (Set.ofArray row[0]) (Set.ofArray row[1]) |> Set.toArray

    let calcPoints correctValues =
        let exponent = (correctValues |> Array.length) - 1
        if exponent = -1 then 0 else int (2.0 ** exponent)

    let rowPoints = rows |> Array.map (fun f -> cardCorrectValues f |> calcPoints)
    let result = rowPoints |> Array.sum
    result
    
let part2 input =
    let rows = Parsing.parseRows input parseRow
    let result = 0
    result
