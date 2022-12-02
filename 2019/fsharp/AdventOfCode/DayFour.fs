module AdventOfCode.DayFour

let inline (/%) x y = (x / y, x % y)

let rec padDigits start value places =
    if places > 10 then
        invalidArg
            "places"
            "A value greater than 10 for places will overflow Int32 in all cases"

    let result =
        if places <= 0
        then start
        else padDigits (start * 10 + value) value (places - 1)

    if result < start then failwith "Overflow" else result

type PasscodeValidityResult =
    | Valid
    | Invalid
    | EarlyExit of remainingDigits: int * failedDigit: int * place: int

let isValidPasscode (passcode: int): PasscodeValidityResult =
    let rec loop work prevDigit chain hasDouble place =
        match work with
        | (0, 0) -> if hasDouble then Valid else Invalid
        | (digits, digit) when digit = prevDigit ->
            loop (digits /% 10) digit (chain + 1) true (place + 1)
        | (digits, digit) when digit <= prevDigit ->
            loop (digits /% 10) digit 1 hasDouble (place + 1)
        | (digits, digit) -> EarlyExit(digits, digit, place)

    loop (passcode /% 10) 10 1 false 1

let isValidPasscodeRedux (passcode: int): PasscodeValidityResult =
    let rec loop work prevDigit chain hasDouble place =
        match work with
        | (0, 0) ->
            if chain = 2 then Valid
            elif hasDouble then Valid
            else Invalid
        | (digits, digit) when digit = prevDigit ->
            loop (digits /% 10) digit (chain + 1) hasDouble (place + 1)
        | (digits, digit) when digit <= prevDigit ->
            let hasDouble' = if chain = 2 then true else hasDouble
            loop (digits /% 10) digit 1 hasDouble' (place + 1)
        | (digits, digit) -> EarlyExit(digits, digit, place)

    loop (passcode /% 10) 10 1 false 1

type Solution() =
    inherit Util.Solution<int>("Day Four", "04.txt")

    let input = {| Start = 124075; Stop = 580769 |}

    let findMatchingPasswords isValidPasscode start stop =
        let rec loop (matches: int list) candidate =
            if candidate > stop then
                matches
            else
                match isValidPasscode candidate with
                | Valid -> loop (candidate :: matches) (candidate + 1)
                | Invalid -> loop matches (candidate + 1)
                | EarlyExit (digits, digit, place) ->
                    loop matches (padDigits digits digit place)

        loop [] start

    override __.Part1() =
        let matches =
            findMatchingPasswords isValidPasscode input.Start input.Stop

        matches.Length

    override __.Part2() =
        let matches =
            findMatchingPasswords isValidPasscodeRedux input.Start input.Stop

        matches.Length
