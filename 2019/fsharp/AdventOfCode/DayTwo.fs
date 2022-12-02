module AdventOfCode.DayTwo

open Intcode
open System.IO

type Solution() as self =
    inherit Util.Solution<int>("Day Two", "02.txt")

    let memory =
        File.ReadAllText(self.InputPath).Split(',')
        |> Array.map int

    member private __.Run(noun: int, verb: int) =
        let computer =
            Computer
                ({ Memory = Array.copy memory
                   Input = 0
                   Output = []
                   IC = 0 })

        computer.[1] <- noun
        computer.[2] <- verb
        computer.Run()
        computer.[0]

    override this.Part1() = this.Run(12, 2)

    override this.Part2() =
        let target = 19690720

        let rec loop noun verb =
            match this.Run(noun, verb) with
            | result when result = target -> 100 * noun + verb
            | _ when noun > 99 -> -1
            | _ when verb > 99 -> loop (noun + 1) 0
            | _ -> loop noun (verb + 1)

        loop 0 0
