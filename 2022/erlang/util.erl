-module(util).
-export([chunk/2, exec/3]).

exec(Input, Part1, Part2) -> 
    {ok, Binary} = file:read_file(Input),
    {P1Time, P1} = timer:tc(Part1, [Binary]),
    {P2Time, P2} = timer:tc(Part2, [Binary]),
    io:fwrite("Part 1: ~p,\tElapsed: ~w ms~n", [P1, P1Time / 1000]),
    io:fwrite("Part 2: ~p,\tElapsed: ~w ms~n", [P2, P2Time / 1000]).

chunk(N, List) ->
    chunk(List, [], [], N, N).

chunk([], Chunk, Acc, _, _) ->
    lists:reverse([lists:reverse(Chunk) | Acc]);
chunk(List, Chunk, Acc, Size, 0) ->
    chunk(List, [], [lists:reverse(Chunk) | Acc], Size, Size);
chunk([H|T], Chunk, Acc, Size, N) ->
    chunk(T, [H|Chunk], Acc, Size, N - 1).
