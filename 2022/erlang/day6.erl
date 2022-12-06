-module(day6).
-export([main/1]).

main([Input]) ->
    util:exec(Input, fun part1/1, fun part2/1).

distinct(Chars) ->
    distinct(Chars, 0).

distinct([], _) -> true;
distinct([Char|Chars], 0) ->
    distinct(Chars, 1 bsl (Char - $a));
distinct([Char|Chars], BitSet) ->
    Value = 1 bsl (Char - $a),
    case BitSet band Value of
        0 -> distinct(Chars, BitSet bor Value);
        _ -> false
    end.

find_marker([], _, _, _) -> throw(no_match);
find_marker([H|T], Window, WindowSize, Count) when length(Window) < WindowSize ->
    find_marker(T, [H|Window], WindowSize, Count + 1);
find_marker([H|T], Window, WindowSize, Count) ->
    NextWindow = [H|lists:droplast(Window)],
    NextCount = Count + 1,
    case distinct(NextWindow) of
        true -> { NextWindow, NextCount };
        false -> find_marker(T, NextWindow, WindowSize, NextCount)
    end.

part1(Input) ->
    find_marker(binary:bin_to_list(string:trim(Input)), [], 4, 0).

part2(Input) ->
    find_marker(binary:bin_to_list(string:trim(Input)), [], 14, 0).
