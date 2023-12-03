-module(day4).
-export([main/1]).

main([Input]) ->
    util:exec(Input, fun part1/1, fun part2/1).

parse_input(Input) ->
    Lines = string:split(string:trim(Input), "\n", all),
    lists:map(fun(Line) ->
        lists:map(fun(Section) ->
            [First, Last] = string:split(Section, "-"),
            {Start, []} = string:to_integer(First),
            {End, []} = string:to_integer(Last),
            sets:from_list(lists:seq(Start, End),  [{version, 2}])
        end, string:split(Line, ","))
    end, Lines).

part1(Input) ->
    Pairs = parse_input(Input),
    Subsets = lists:filter(fun([Section1, Section2]) ->
        case sets:size(Section1) < sets:size(Section2) of 
            true -> sets:is_subset(Section1, Section2);
            false -> sets:is_subset(Section2, Section1)
        end
    end, Pairs),
    length(Subsets).

part2(Input) ->
    Pairs = parse_input(Input),
    Intersections = lists:filter(fun([Section1, Section2]) ->
        not sets:is_empty(sets:intersection(Section1, Section2))
    end, Pairs),
    length(Intersections).
