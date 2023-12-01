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
let ``D1`` () =
    let input = """
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
"""
    let pt1 = D1.part1 input
    Assert.Equal(142, pt1)
    
    Assert.Equal(28, D1.part2 "two1nineight")

    let input2 = """
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
"""
    let pt2 = D1.part2 input2
    Assert.Equal(281, pt2)
