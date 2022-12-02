-module(util).
-export([exec/3, stopwatch/1]).

stopwatch(Thunk) ->
    Start = erlang:monotonic_time(millisecond),
    Result = Thunk(),
    End = erlang:monotonic_time(millisecond),
    {Result, End - Start}.

exec(Input, Part1, Part2) -> 
    {ok, Binary} = file:read_file(Input),
    {P1, P1Time} = stopwatch(fun() -> Part1(Binary) end),
    {P2, P2Time} = stopwatch(fun() -> Part2(Binary) end),
    io:fwrite("Part 1: ~w,\tElapsed: ~w ms~n", [P1, P1Time]),
    io:fwrite("Part 2: ~w,\tElapsed: ~w ms~n", [P2, P2Time]).
