-module(day2).
-export([main/1]).

main([Input]) ->
    util:exec(Input, fun part1/1, fun part2/1).

parse_input(Input) ->
    Lines = string:split(string:trim(Input), "\n", all),
    lists:map(fun(Line) -> string:lexemes(Line, " ") end, Lines).

calculate_score([Opponent, Player]) ->
    case [Opponent, Player] of
        [<<"A">>, <<"X">>] -> 4;
        [<<"A">>, <<"Y">>] -> 8;
        [<<"A">>, <<"Z">>] -> 3;
        [<<"B">>, <<"X">>] -> 1;
        [<<"B">>, <<"Y">>] -> 5;
        [<<"B">>, <<"Z">>] -> 9;
        [<<"C">>, <<"X">>] -> 7;
        [<<"C">>, <<"Y">>] -> 2;
        [<<"C">>, <<"Z">>] -> 6;
        _ -> throw([no_match, [Opponent, Player]])
    end.

determine_move([Opponent, Outcome]) ->
    case [Opponent, Outcome] of
        [<<"A">>, <<"X">>] -> <<"Z">>;
        [<<"A">>, <<"Y">>] -> <<"X">>;
        [<<"A">>, <<"Z">>] -> <<"Y">>;
        [<<"B">>, <<"X">>] -> <<"X">>;
        [<<"B">>, <<"Y">>] -> <<"Y">>;
        [<<"B">>, <<"Z">>] -> <<"Z">>;
        [<<"C">>, <<"X">>] -> <<"Y">>;
        [<<"C">>, <<"Y">>] -> <<"Z">>;
        [<<"C">>, <<"Z">>] -> <<"X">>;
        _ -> throw([no_match, [Opponent, Outcome]])
    end.

part1(Input) ->
    Rounds = parse_input(Input),
    Scores = lists:map(fun calculate_score/1, Rounds),
    lists:sum(Scores).

part2(Input) ->
    Rounds = parse_input(Input),
    Scores = lists:map(
        fun (Round) ->
            [Opponent, _] = Round,
            calculate_score([Opponent, determine_move(Round)])
        end,
        Rounds
    ),
    lists:sum(Scores).
