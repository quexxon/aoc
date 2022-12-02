module AdventOfCode.Tests.DayFour

open AdventOfCode.DayFour
open Expecto
open System

[<Tests>]
let testPadDigits =
    testList
        "Advent of Code - Day Four - padDigits"
        [ test "Works for 0" {
              let actual, expected = padDigits 0 0 0, 0
              Expect.equal actual expected "Padding 0 with 0 zeroes should be 0"
          }
          test "Works for single digits with 0" {
              let actual, expected = padDigits 0 1 1, 1
              Expect.equal actual expected "Padding 0 with 1 ones should be 1"
          }
          test "Works for multiple digits with 0" {
              let actual, expected = padDigits 0 1 8, 11111111
              Expect.equal actual expected "Padding 0 with 8 ones should be 11111111"
          }
          test "Works for single digit starting values" {
              let actual, expected = padDigits 8 5 3, 8555
              Expect.equal actual expected "Padding 8 with 3 fives should be 8555"
          }
          test "Works for mutli-digit starting values" {
              let actual, expected = padDigits 46 7 5, 4677777
              Expect.equal actual expected "Padding 46 with 5 sevens should be 4677777"
          }
          test "Fails when places is greater than 10" {
              Expect.throws (fun () -> padDigits 0 0 11 |> ignore)
                  "A places value greater than 10 should throw"
          }
          test "Fails on overflow" {
              Expect.throws (fun () -> padDigits Int32.MaxValue 0 1 |> ignore)
                  "Throws on overflow"
          } ]

[<Tests>]
let testDivRem =
    testList
        "Advent of Code - Day Four - /%"
        [ test "Fails on division by zero" {
              Expect.throws (fun () -> 1 /% 0 |> ignore) "Division by zero should throw" } ]

[<Tests>]
let testIsValidPasscodeRedux =
    testList
        "Advent of Code - Day Four - isValidPasscode"
        [ test "Fails with single digit numbers" {
              let actual, expected = isValidPasscodeRedux 5, Invalid
              Expect.equal actual expected "All single digit numbers are invalid"
          }
          test "Works with any two digit number with doubles" {
              let actual, expected = isValidPasscodeRedux 55, Valid
              Expect.equal actual expected "All two digit duplicates are valid"
          }
          test "Advent of Code example 1" {
              let actual, expected = isValidPasscodeRedux 112233, Valid
              Expect.equal actual expected "112233 is a valid passcode"
          }
          test "Advent of Code example 2" {
              let actual, expected = isValidPasscodeRedux 123444, Invalid
              Expect.equal actual expected "123444 is an invalid passcode"
          }
          test "Advent of Code example 3" {
              let actual, expected = isValidPasscodeRedux 111122, Valid
              Expect.equal actual expected "111122 is a valid passcode"
          }
          test "Fails with a even numbered large group of duplicates" {
              let actual, expected = isValidPasscodeRedux 123333, Invalid
              Expect.equal actual expected "123333 is an invalid passcode"
          } ]
