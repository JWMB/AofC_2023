module D04

open Tools

type Card = { Id: int; Winning: int array; Mine: int array }

let parseRow (row: string) =
    let row = row.Split(':');
    let numbers = row[1].Split('|')
                |> Array.map (fun f -> 
                        f.Trim().Split(' ')
                        |> Array.map (fun v -> v.Trim())
                        |> Array.filter (fun v -> v.Length > 0)
                        |> Array.map int)
        
    { 
        Id = row[0].Split(' ') |> Array.last |> int;
        Winning = numbers[0];
        Mine = numbers[1]
    }


let part1 input =
    let rows = Parsing.parseRows input parseRow

    let cardCorrectValues row =
        Set.intersect (Set.ofArray row.Winning) (Set.ofArray row.Mine) |> Set.toArray

    let calcPoints correctValues =
        let exponent = (correctValues |> Array.length) - 1
        if exponent = -1 then 0 else int (2.0 ** exponent)

    let rowPoints = rows |> Array.map (fun f -> cardCorrectValues f |> calcPoints)
    let result = rowPoints |> Array.sum
    result
    
let part2 input =
    let rows = Parsing.parseRows input parseRow
    let cardCounts = rows |> Array.map (fun f -> 1) //(fun f -> (f.Id, 1)) |> Map.ofArray 

    let cardNumCorrectValues row =
        Set.intersect (Set.ofArray row.Winning) (Set.ofArray row.Mine) |> Set.count


    rows |> Array.iteri (fun rowIndex row ->
                    let numCorrect = cardNumCorrectValues row
                    let copiesOfThisCard = cardCounts.[rowIndex]
                    [| 1..numCorrect|] |> Array.iter (fun i -> cardCounts.[rowIndex + i] <- cardCounts.[rowIndex + i] + copiesOfThisCard)
                )

    let result = cardCounts |> Array.sum
    result
