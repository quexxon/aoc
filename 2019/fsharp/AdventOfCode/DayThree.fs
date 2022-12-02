module AdventOfCode.DayThree

open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

type Point =
    { X: int
      Y: int }

    static member Origin = { X = 0; Y = 0 }

type Segment =
    { P1: Point
      P2: Point
      Line: int }

    member this.Normalize() =
        if this.IsHorizontal() then
            if this.P1.X > this.P2.X then
                { this with P1 = this.P2; P2 = this.P1 }
            else
                this
        elif this.IsVertical() then
            if this.P1.Y > this.P2.Y then
                { this with P1 = this.P2; P2 = this.P1 }
            else
                this
        else
            failwith "Invalid segment"

    member this.Intersects(that: Segment): bool =
        match (this.IsHorizontal(), that.IsHorizontal()) with
        | (true, true) ->
            (that.P1.X >= this.P1.X && that.P1.X <= this.P2.X)
            || (that.P2.X >= this.P1.X && that.P2.X <= this.P2.X)
        | (false, false) ->
            (that.P1.Y >= this.P1.Y && that.P1.Y <= this.P2.Y)
            || (that.P2.Y >= this.P1.Y && that.P2.Y <= this.P2.Y)
        | (true, false) ->
            this.P1.X
            <= that.P1.X
            && this.P2.X >= that.P1.X
            && this.P1.Y >= that.P1.Y
            && this.P1.Y <= that.P2.Y
        | (false, true) ->
            that.P1.X
            <= this.P1.X
            && that.P2.X >= this.P1.X
            && that.P1.Y >= this.P1.Y
            && that.P1.Y <= this.P2.Y

    member this.Contains(point: Point): bool =
        if this.IsHorizontal() then
            point.Y = this.P1.Y
            && point.X >= this.P1.X
            && point.X <= this.P2.X
        else
            point.X = this.P1.X
            && point.Y >= this.P1.Y
            && point.Y <= this.P2.Y

    member this.IsHorizontal() =
        this.P1.X <> this.P2.X && this.P1.Y = this.P2.Y

    member this.IsVertical() =
        this.P1.X = this.P2.X && this.P1.Y <> this.P2.Y

    member this.Length =
        sqrt
            ((float this.P2.X - float this.P1.X)
             ** 2.
             + (float this.P2.Y - float this.P1.Y) ** 2.)
        |> int

type Heading =
    | North
    | East
    | South
    | West

type Direction = { Heading: Heading; Distance: int }

module Direction =
    let parse direction =
        let m =
            Regex.Match(direction, @"^(?<heading>[UDLR])(?<distance>\d+)$")

        if m.Success then
            { Heading =
                  match m.Groups.["heading"].Value with
                  | "U" -> North
                  | "R" -> East
                  | "D" -> South
                  | "L" -> West
                  | _ -> failwith "Invalid heading"
              Distance = m.Groups.["distance"].Value |> int }
        else
            failwith "Invalid direction format"

let segmentFromDirection start line direction =
    let { Heading = heading; Distance = distance } = Direction.parse direction

    { P1 = start
      P2 =
          match heading with
          | North -> { X = start.X; Y = start.Y + distance }
          | East -> { X = start.X + distance; Y = start.Y }
          | South -> { X = start.X; Y = start.Y - distance }
          | West -> { X = start.X - distance; Y = start.Y }
      Line = line }

let parsePath (path: seq<string>) (line: int): List<Segment> =
    let segments = List()
    let mutable prevPoint = Point.Origin

    for direction in path do
        let segment =
            segmentFromDirection prevPoint line direction

        segments.Add(segment.Normalize())
        prevPoint <- segment.P2

    segments

type Solution() as self =
    inherit Util.Solution<int>("Day Three", "03.txt")

    let input =
        File.ReadLines(self.InputPath) |> Array.ofSeq

    member __.FindNearestIntersection(calcDistance) =
        let horizontalSegments = SortedSet()
        let verticalSegments = SortedSet()
        let activeHorizontals = SortedSet()
        let mutable nearest = None

        let rec prepareActiveSet vert =
            if horizontalSegments.Count = 0 then
                ()
            elif horizontalSegments.Min.P2.X < vert.P1.X then
                horizontalSegments.Remove(horizontalSegments.Min)
                |> ignore

                prepareActiveSet vert
            elif horizontalSegments.Min.P1.X <= vert.P1.X then
                activeHorizontals.Add(horizontalSegments.Min)
                |> ignore

                horizontalSegments.Remove(horizontalSegments.Min)
                |> ignore

                prepareActiveSet vert
            else
                activeHorizontals.RemoveWhere(fun segment -> segment.P2.X < vert.P1.X)
                |> ignore

        let processLine index (path: string) = parsePath (path.Split(',')) index

        let lines = Array.mapi processLine input

        for line in lines do
            for segment in line do
                if segment.IsHorizontal() then
                    horizontalSegments.Add(segment)
                else
                    verticalSegments.Add(segment)
                |> ignore

        for vert in verticalSegments do
            prepareActiveSet vert

            for horiz in activeHorizontals do
                if vert.Line <> horiz.Line && vert.Intersects(horiz) then
                    let intersection = { X = vert.P1.X; Y = horiz.P1.Y }

                    if intersection <> Point.Origin then
                        let distance = calcDistance lines intersection

                        match nearest with
                        | Some current ->
                            if distance < current then nearest <- Some distance
                        | None -> nearest <- Some distance

        nearest.Value

    override this.Part1() =
        let manhattanDistance _ point = abs point.X + abs point.Y
        this.FindNearestIntersection(manhattanDistance)

    override this.Part2() =
        let stepsToPoint point (line: seq<Segment>) =
            use e = line.GetEnumerator()

            let rec walk steps =
                if e.Current.Contains(point) then
                    steps + { e.Current with P2 = point }.Length
                else
                    let steps' = steps + e.Current.Length

                    if e.MoveNext() then
                        walk steps'
                    else
                        failwith "Point doesn't occur on line"

            if e.MoveNext() then walk 0 else failwith "Empty line"

        let stepsFromOrigin lines point =
            Seq.sumBy (stepsToPoint point) lines

        this.FindNearestIntersection(stepsFromOrigin)
