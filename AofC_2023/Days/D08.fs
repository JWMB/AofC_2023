module D08

open Tools
open System.Text.RegularExpressions
open System

type Node = { Name: string; Connections: Node array }

let parseRow (row: string) =
    let chunks = row.Split('=')
    (chunks[0].Trim(), Regex.Matches(chunks[1], @"\w+") |> Seq.toArray |> Array.map (fun f -> f.Value))

let constructNodes (rows: string)  =
    let nodeDefs = rows.Split('\n') |> Array.map parseRow |> Map.ofArray
    //let nodeMap = nodeDefs |> Map.ofArray (fun (id, conns) -> (id, [||])) //Array.map (fun f -> { Name = fst f; Connections = [||] }) |> Map.ofArray (fun f -> f)
    nodeDefs


let part1 input =
    let input = Parsing.cleanWithTrimEmptyLines input
    let chunks = Regex.Split(input, "\n\n");
    let turns = chunks[0]
    let nodes = constructNodes chunks[1]

    let rec loop currentNode stepIndex =
        let turn = turns[stepIndex % turns.Length]
        let turnArrIndex = if turn = 'L' then 0 else 1
        let nextNodeId = nodes[currentNode][turnArrIndex]
        if nextNodeId = "ZZZ" then stepIndex
        else loop nextNodeId (stepIndex + 1)
        
    let numSteps = 1 + loop "AAA" 0
    numSteps

    
let part2 input =
    let input = Parsing.cleanWithTrimEmptyLines input
    let chunks = Regex.Split(input, "\n\n");
    let turns = chunks[0] |> Seq.toArray |> Array.map (fun turn -> if turn = 'L' then 0 else 1)
    let nodes = constructNodes chunks[1]

    let endNodes = nodes |> Map.toArray |> Array.map (fun f -> fst f) |> Array.filter (fun f -> f.EndsWith "Z")

    let rec getIndicesWhenAtEnd currentNode stepIndex (visitHistory: System.Collections.Generic.List<System.Tuple<string, int>>) =
        seq {
            let indexMod = stepIndex % turns.Length
            let turn = turns[indexMod]
            let stateId = (currentNode, indexMod)
            if visitHistory.Contains stateId then
                yield stepIndex
            else
                visitHistory.Add(stateId)

                let nextNodeId = nodes[currentNode][turn]
                if endNodes |> Array.contains nextNodeId then // if nextNodeId.EndsWith("Z") then
                    yield stepIndex
                yield! getIndicesWhenAtEnd nextNodeId (stepIndex + 1) visitHistory
        }
    
    let initialState = nodes |> Map.toArray |> Array.map (fun (k, _) -> k) |> Array.filter (fun k -> k.EndsWith("A"))
    
    let leastCommonDenominator (values: int array) =
        let factorialize value =
            let max = int (Math.Sqrt (double value))
            let rec loop v (den: int) = seq {
                if den > max then
                    yield v
                else if v > 1 then
                    let isDivisible = v % den = 0
                    if isDivisible then
                        yield den

                    let nextDen = den + if isDivisible then 0 else (if den > 2 then 2 else 1)
                    let nextV = if isDivisible then v / den else v
                    yield! loop nextV nextDen

            }
            loop value 2 |> Seq.toArray
        let valueFactors = values |> Array.map factorialize

        let allDistinctFactors = valueFactors |> Array.reduce Array.append |> Array.distinct
        let countsByFactor =
            allDistinctFactors
            |> Array.map (fun f -> {|
                                    Factor = f; 
                                    Counts = valueFactors |> Array.map (fun arr -> arr |> Array.filter (fun v -> v = f) |> Array.length)
                                    |})

        let common =
            countsByFactor
            |> Array.map (fun f -> 
                    let min = f.Counts |> Array.min
                    if min = 0 then 1 else f.Factor * min
                    )
            |> Array.reduce (fun a b -> a * b)
        common

    //let aaa = leastCommonDenominator [| 22420; 18115; 13204; 24255; 14434; 16273|]
    //let aaa = leastCommonDenominator [| 12; 42; 256 *3  |] 


    let fullLoops =
        initialState
        |> Array.map (fun f -> getIndicesWhenAtEnd f 0 (new System.Collections.Generic.List<System.Tuple<string, int>>()))
        |> Array.map (fun f -> f |> Seq.toArray)
        |> Array.map (fun f -> {| Length = f |> Array.last; Goals = f[0..f.Length-2] |})
    
    // Example: 
    // period 6, goal 3 = 3 + 6a
    //  3 9 15 21 27 33 39
    // period 4, goal 1 = 1 + 4b
    //  1 5 9 13 17 21 25 29 33
    // period 9, goal 5 = 9 + 5c
    //  5 14 23 32 41
    // Find first when all 3 are the same

    let longestPeriod = fullLoops |> Array.maxBy(fun f -> f.Length)
    // Length: 5 / 1, 3
    let rec findCommon iteration =
        let adjustedIndices = fullLoops |> Array.map (fun f -> f.Goals |> Array.map (fun x -> x + iteration * f.Length))
        if true then findCommon iteration + 1
        else 0

    //findCommon 0

    0