module AdventOfCode.DayOne

open System.IO

let fuelByMass mass = int (mass / 3.0) - 2

let correctFuelByMass mass =
    let rec loop sum mass =
        match fuelByMass (float mass) with
        | fuel when fuel <= 0 -> sum
        | fuel -> loop (sum + fuel) fuel

    loop 0 mass

type Solution() as self =
    inherit Util.Solution<int>("Day One", "01.txt")
    let inputLines = File.ReadLines self.InputPath

    override this.Part1() =
        Seq.sumBy (float >> fuelByMass) inputLines

    override this.Part2() =
        Seq.sumBy (int >> correctFuelByMass) inputLines
