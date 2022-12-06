-module(util).
-export([chunk/2, exec/3, stopwatch/1]).

stopwatch(Thunk) ->
    Start = erlang:monotonic_time(microsecond),
    Result = Thunk(),
    End = erlang:monotonic_time(microsecond),
    {Result, (End - Start) / 1000}.

exec(Input, Part1, Part2) -> 
    {ok, Binary} = file:read_file(Input),
    {P1, P1Time} = stopwatch(fun() -> Part1(Binary) end),
    {P2, P2Time} = stopwatch(fun() -> Part2(Binary) end),
    io:fwrite("Part 1: ~p,\tElapsed: ~w ms~n", [P1, P1Time]),
    io:fwrite("Part 2: ~p,\tElapsed: ~w ms~n", [P2, P2Time]).

chunk(N, List) ->
    chunk(List, [], [], N, N).

chunk([], Chunk, Acc, _, _) ->
    lists:reverse([lists:reverse(Chunk) | Acc]);
chunk(List, Chunk, Acc, Size, 0) ->
    chunk(List, [], [lists:reverse(Chunk) | Acc], Size, Size);
chunk([H|T], Chunk, Acc, Size, N) ->
    chunk(T, [H|Chunk], Acc, Size, N - 1).
