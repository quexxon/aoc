-module(day1).
-export([main/1]).

main([Input]) ->
    util:exec(Input, fun part1/1, fun part2/1).

parse_input(Input) ->
    lists:map(
        fun(Group) ->
            lists:sum(lists:map(
                fun(S) -> {N, _} = string:to_integer(S), N end,
                string:lexemes(Group, "\n")
            ))
        end,
        string:split(Input, "\n\n", all)
    ).

part1(Input) ->
    Sums = parse_input(Input),
    lists:max(Sums).

part2(Input) ->
    Sums = parse_input(Input),
    lists:sum(lists:sublist(lists:reverse(lists:sort(Sums)), 3)).
