module D07

open Tools

// TODO: figure out type definitions
//type Hand = int array with
//    member this.compare other = true

//type 'T ``[]`` with
//    member a.Last = a.[a.Length - 1]
////type Hand = { Cards: int array }
//type FullHouse =
//    inherit 'T
//    member this.X = 0


type Rpund = { Cards: int array; Bet: int }
let parseRow (row: string) =
    let split = row.Split ' ' 
    { 
        Bet = int split[1];
        Cards = split[0]
            |> Seq.map (
            fun c -> 
                match c with
                | 'A' -> 14
                | 'K' -> 13
                | 'Q' -> 12
                | 'J' -> 11
                | 'T' -> 10
                | other -> int other - 48)
            |> Seq.toArray
    }

type HandType =
    | HighCard = 0
    | OnePair = 1
    | TwoPairs = 2
    | ThreeOfAKind = 3
    | FullHouse = 4
    | FourOfAKind = 5
    | FiveOfAKind = 6

let getHandType hand =
    let countBy = hand |> Array.groupBy (fun f -> f) |> Array.map (fun (k, v) -> (k, v.Length))
    let numPairs countBy = countBy |> Array.filter (fun (k, v) -> v = 2) |> Array.length
    let numDifferentCards = countBy.Length
    match numDifferentCards with
        | 5 -> HandType.HighCard
        | 4 -> HandType.OnePair
        | 3 ->
            match numPairs countBy with
                | 2 -> HandType.TwoPairs
                | 0 -> HandType.ThreeOfAKind
                | _ -> failwith "Error 3"
        | 2 ->
            if countBy |> Array.exists (fun (_, v) -> v = 3) then
                HandType.FullHouse
            else
                HandType.FourOfAKind
        | 1 -> HandType.FiveOfAKind
        | _ -> failwith "Error"

let handComparer hand1 hand2 =
    let type1 = getHandType hand1
    let type2 = getHandType hand2
    let diff = int type2 - int type1

    let signOf value
        = match value with
            | v when v > 0 -> 1
            | v when v < 0 -> -1
            | _ -> 0

    match signOf diff with
        | 0 ->
            let rec loop vals1 vals2 =
                let sign = signOf ((vals2 |> Array.head) - (vals1 |> Array.head))
                if sign = 0 then
                    if vals1.Length = 1 then 0
                    else
                        loop (vals1 |> Array.tail) (vals2 |> Array.tail)
                else
                    sign
            loop hand1 hand2
        | v -> v

let part1 input =
    let rows = Parsing.parseRows input parseRow
    let ranked = rows |> Array.sortWith (fun a b -> handComparer a.Cards b.Cards) |> Array.rev |>  Array.mapi (fun i v -> {| Rank = i + 1; Hand = v |})
    let winnings = ranked |> Array.map (fun v -> v.Rank * v.Hand.Bet)
    let result = winnings |> Array.sum
    result
    
let part2 input =
    let rows = Parsing.parseRows input parseRow
    let result = 0
    result
