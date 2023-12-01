module D1

open Tools
open System.Text.RegularExpressions

let firstLast arr =
    let len = arr |> Array.length
    let firstLastX = 
        match len with
             | 0 -> "0"
             | _ -> $"{arr[0]}{arr[len - 1]}" // WTF? works in debugger but not in code!? $"{digits[0]}{digits[^1]}"
        
    firstLastX |> int


let part1 input =
    let parseRow input =
        let digits = Regex.Replace(input, @"\D", "") |> Seq.toArray
        firstLast digits
        //let firstLastX = 
        //    match digits.Length with
        //     | 0 -> "0"
        //     | _ -> $"{digits[0]}{digits[digits.Length - 1]}" // WTF? works in debugger but not in code!? $"{digits[0]}{digits[^1]}"
        //firstLastX |> int

    let rowValues = Parsing.parseRows input parseRow
    Seq.sum rowValues

let part2 input =
    let digitWords = "zero one two three four five six seven eight nine".Split ' '
    let digitWordPattern = digitWords |> String.concat "|"
    let pattern = $@"({digitWordPattern}|\d)"

    let makeDigit str =
        match digitWords |> Array.tryFindIndex (fun f -> f = str) with
            | Some s -> s
            | _ -> int str

    let parseRow input =
        let optionalFinal = Regex.Match(input, $"{pattern}$")
        let matches = Regex.Matches(input, pattern) |> Seq.toArray
        let matches = 
            if optionalFinal.Success && optionalFinal.Value <> (matches |> Array.last).Value then
                [| optionalFinal |] |> Array.append matches
            else
                matches
        let values = matches |> Array.map (fun f -> makeDigit(f.Value))
        firstLast values

    let rowValues = Parsing.parseRows input parseRow
    let result = Seq.sum rowValues
    result // 55309 too high
