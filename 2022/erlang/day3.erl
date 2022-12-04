-module(day3).
-export([main/1]).

main([Input]) ->
    util:exec(Input, fun part1/1, fun part2/1).

parse_input_part1(Input) ->
    Lines = string:split(string:trim(Input), "\n", all),
    lists:map(fun(Line) ->
        {Front, Back} = lists:split(byte_size(Line) div 2, binary_to_list(Line)),
        {
            sets:from_list(Front, [{version, 2}]), 
            sets:from_list(Back, [{version, 2}])
        }
    end, Lines).

parse_input_part2(Input) ->
    Lines = string:split(string:trim(Input), "\n", all),
    Groups = util:chunk(3, Lines),
    lists:map(fun(Group) ->
        lists:map(fun(Entry) ->
            sets:from_list(binary_to_list(Entry), [{version, 2}])
        end, Group)
    end, Groups).

char_value(Char) when Char >= $\a, Char =< $\z -> Char - $\a + 1;
char_value(Char) when Char >= $\A, Char =< $\Z -> Char - $\A + 27;
char_value(Char) -> throw({invalid_char, Char}).

part1(Input) -> 
    Rucksacks = parse_input_part1(Input),
    Values = lists:map(fun({Front, Back}) ->
        [Shared] = sets:to_list(sets:intersection(Front, Back)),
        char_value(Shared)
    end, Rucksacks),
    lists:sum(Values).

part2(Input) ->
    Groups = parse_input_part2(Input),
    Values = lists:map(fun([Bag1, Bag2, Bag3]) ->
        Intersection = sets:intersection(sets:intersection(Bag1, Bag2), Bag3),
        [Common] = sets:to_list(Intersection),
        char_value(Common)
    end, Groups),
    lists:sum(Values).
