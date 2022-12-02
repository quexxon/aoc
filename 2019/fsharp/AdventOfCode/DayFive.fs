module AdventOfCode.DayFive

open Intcode
open System.IO

type Solution() as self =
    inherit Util.Solution<int>("Day Five", "05.txt")

    let memory =
        File.ReadAllText(self.InputPath).Split(',')
        |> Array.map int

    member private __.Run(input: int) =
        let computer =
            Computer
                ({ Memory = Array.copy memory
                   Input = input
                   Output = []
                   IC = 0 })

        computer.Run()

        match computer.Output with
        | [ diagnosticCode ] -> diagnosticCode
        | diagnosticCode :: testCodes ->
            if List.forall ((<>) 0) testCodes
            then failwith $"Some tests failed: {testCodes}"
            diagnosticCode
        | [] -> failwith "Empty output"

    override this.Part1() = this.Run(1)

    override this.Part2() = this.Run(5)
