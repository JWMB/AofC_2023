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
"""
    let pt1 = D1.part1 input
    Assert.Equal(24000, pt1)
    
    //let pt2 = D1.part2 input
    //Assert.Equal(45000, pt2)
