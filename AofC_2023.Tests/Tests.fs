module Tests

open System
open Xunit

[<Fact>]
let ``template`` () =
    let input = """
"""
    Assert.Equal(0, Template.part1 input)
    Assert.Equal(0, Template.part2 input)


[<Fact>]
let ``D01`` () =
    let input = """
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
"""
    let pt1 = D01.part1 input
    Assert.Equal(142, pt1)
    
    Assert.Equal(28, D01.part2 "two1nineight")

    let input2 = """
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
"""
    let pt2 = D01.part2 input2
    Assert.Equal(281, pt2)


[<Fact>]
let ``D02`` () =
    let input = """
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
"""
    Assert.Equal(8, D02.part1 input)
    Assert.Equal(2286, D02.part2 input)


[<Fact>]
let ``D03`` () =
    let input = """
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
"""
    Assert.Equal(4361, D03.part1 input)
    Assert.Equal(467835, D03.part2 input)



[<Fact>]
let ``D04`` () =
    let input = """
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
"""
    Assert.Equal(13, D04.part1 input)
    Assert.Equal(30, D04.part2 input)




[<Fact>]
let ``D05`` () =
    let input = """
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
"""
    Assert.Equal(35, D05.part1 input |> int)
    Assert.Equal(0, D05.part2 input |> int)




[<Fact>]
let ``D06`` () =
    let input = """
Time:      7  15   30
Distance:  9  40  200
"""
    Assert.Equal(288, D06.part1 input)
    Assert.Equal(71503, D06.part2 input)




[<Fact>]
let ``D07`` () =
    let input = """
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
"""

    Assert.Equal(6440, D07.part1 input)
    Assert.Equal(5905, D07.part2 input)



[<Fact>]
let ``D08`` () =
    let input = """
RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
"""
    Assert.Equal(2, D08.part1 input)

    let input = """
LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
"""
    Assert.Equal(6, D08.part1 input)


//    let input = """
//LR

//11A = (11B, XXX)
//11B = (XXX, 11Z)
//11Z = (11B, XXX)
//22A = (22B, XXX)
//22B = (22C, 22C)
//22C = (22Z, 22Z)
//22Z = (22B, 22B)
//XXX = (XXX, XXX)
//"""
    //Assert.Equal(6, D08.part2 input)




[<Fact>]
let ``D09`` () =
    let input = """
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
"""
    Assert.Equal(114L, D09.part1 input)
    Assert.Equal(2L, D09.part2 input)


