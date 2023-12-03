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
"""
    Assert.Equal(0, D04.part1 input)
    Assert.Equal(0, D04.part2 input)


