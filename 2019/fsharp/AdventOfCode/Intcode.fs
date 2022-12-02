module AdventOfCode.Intcode

type ParameterMode =
    | Position
    | Immediate

module ParameterMode =
    let parse (parameter: int): ParameterMode =
        if parameter = 0 then Position else Immediate

type ParameterPosition =
    | First = 1
    | Second = 2
    | Third = 3

type Parameter =
    { Mode: ParameterMode
      Position: ParameterPosition }

type Operation =
    | Add
    | Multiply
    | Halt
    | Read
    | Write
    | JumpIfTrue
    | JumpIfFalse
    | LessThan
    | Equals

type Instruction =
    { Operation: Operation
      Parameters: {| A: Parameter
                     B: Parameter
                     C: Parameter |}
      Length: int }

type Opcode =
    { Code: int
      Operation: Operation
      Length: int }

module Instruction =
    let private opcodes =
        [ { Code = 1
            Operation = Add
            Length = 4 }
          { Code = 2
            Operation = Multiply
            Length = 4 }
          { Code = 3
            Operation = Read
            Length = 2 }
          { Code = 4
            Operation = Write
            Length = 2 }
          { Code = 5
            Operation = JumpIfTrue
            Length = 3 }
          { Code = 6
            Operation = JumpIfFalse
            Length = 3 }
          { Code = 7
            Operation = LessThan
            Length = 4 }
          { Code = 8
            Operation = Equals
            Length = 4 }
          { Code = 99
            Operation = Halt
            Length = 1 } ]
        |> List.map (fun opcode -> opcode.Code, opcode)
        |> Map.ofList

    let parse (instruction: int): Instruction =
        let opcode = Map.find (instruction % 100) opcodes
        let parameterModes = instruction / 100
        { Operation = opcode.Operation
          Parameters =
              {| A =
                     { Position = ParameterPosition.First
                       Mode = ParameterMode.parse (parameterModes &&& 1) }
                 B =
                     { Position = ParameterPosition.Second
                       Mode = ParameterMode.parse (parameterModes &&& 10) }
                 C =
                     { Position = ParameterPosition.Third
                       Mode = ParameterMode.parse (parameterModes &&& 100) } |}
          Length = opcode.Length }

type State =
    { Memory: int []
      mutable Input: int
      mutable Output: int list
      mutable IC: int }

type Computer(state: State) =
    member private this.Read(parameter: Parameter) =
        match parameter.Mode with
        | Position -> this.[this.[state.IC + int parameter.Position]]
        | Immediate -> this.[state.IC + int parameter.Position]

    member private this.Write(parameter: Parameter, value: int) =
        match parameter.Mode with
        | Position -> this.[this.[state.IC + int parameter.Position]] <- value
        | Immediate -> failwith "Invalid parameter mode for writing"

    member private __.IncrementIC(instruction: Instruction) =
        state.IC <- state.IC + instruction.Length

    member __.Input
        with get () = state.Input
        and set value = state.Input <- value

    member __.Output = state.Output

    member __.Item
        with get (index) = state.Memory.[index]
        and set index value = state.Memory.[index] <- value

    member this.Run() =
        let rec loop () =
            let instruction = Instruction.parse this.[state.IC]
            let parameters = instruction.Parameters
            match instruction.Operation with
            | Halt -> ()
            | Add ->
                let x = this.Read parameters.A
                let y = this.Read parameters.B
                this.Write(parameters.C, x + y)
                this.IncrementIC instruction
                loop ()
            | Multiply ->
                let x = this.Read parameters.A
                let y = this.Read parameters.B
                this.Write(parameters.C, x * y)
                this.IncrementIC instruction
                loop ()
            | Read ->
                this.Write(parameters.A, state.Input)
                this.IncrementIC instruction
                loop ()
            | Write ->
                state.Output <- (this.Read parameters.A) :: state.Output
                this.IncrementIC instruction
                loop ()
            | JumpIfTrue ->
                if (this.Read parameters.A) <> 0
                then state.IC <- this.Read parameters.B
                else this.IncrementIC instruction
                loop ()
            | JumpIfFalse ->
                if (this.Read parameters.A) = 0
                then state.IC <- this.Read parameters.B
                else this.IncrementIC instruction
                loop ()
            | LessThan ->
                let x = this.Read parameters.A
                let y = this.Read parameters.B
                this.Write(parameters.C, (if x < y then 1 else 0))
                this.IncrementIC instruction
                loop ()
            | Equals ->
                let x = this.Read parameters.A
                let y = this.Read parameters.B
                this.Write(parameters.C, (if x = y then 1 else 0))
                this.IncrementIC instruction
                loop ()

        loop ()
