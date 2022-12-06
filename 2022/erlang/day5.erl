-module(day5).
-export([main/1]).

main([Input]) ->
    util:exec(Input, fun part1/1, fun part2/1).

parse_crates(Crates) ->
    { ok, RegEx } = re:compile("(?: ?[ \\[](.)[ \\]])"),
    [Labels|Rows] = lists:reverse(string:split(Crates, "\n", all)),
    {match, Capture} = re:run(Labels, ".+(\\d)", [{capture, all_but_first, binary}]),
    Size = case string:to_integer(Capture) of
        {error, Reason} -> throw(Reason);
        {N,_} -> N
    end,
    lists:foldl(fun(Row, Acc) ->
        case re:run(Row, RegEx, [global, {capture, all_but_first, binary}]) of
            {match, Captured} ->
                lists:foldl(fun({Index, [Match]}, Array) ->
                    case string:trim(Match) == <<"">> of
                        true -> Array;
                        false ->
                            Stack = array:get(Index, Array),
                            array:set(Index, [Match | Stack], Array)
                    end
                end, Acc, lists:enumerate(0, Captured));
            nomatch -> Acc
        end
    end, array:new(Size, {default, []}), Rows).

parse_procedure(Procedure) ->
    { ok, RegEx } = re:compile("^move (\\d+) from (\\d+) to (\\d+)$"),
    Operations = string:split(string:trim(Procedure), "\n", all),
    lists:map(fun(Operation) ->
        { match, Captures } = re:run(Operation, RegEx, [{capture, all_but_first, binary}]),
        lists:map(fun(Capture) ->
            case string:to_integer(Capture) of
                {error, Reason} -> throw(Reason);
                {N,_} -> N
            end
        end, Captures)
    end, Operations).

parse_input(Input) ->
    [Crates, Procedure] = string:split(Input, "\n\n", all),
    {parse_crates(Crates), parse_procedure(Procedure)}.

apply_procedure(Crates, Procedure, MoveCargo) ->
    FinalState = lists:foldl(fun ([Quantity, Origin, Destination], Acc) ->
        Orig = array:get(Origin - 1, Acc),
        Dest = array:get(Destination - 1, Acc),
        {Cargo, NextOrig} = lists:split(Quantity, Orig),
        NextDest = MoveCargo(Cargo, Dest),
        array:set(Destination - 1, NextDest, array:set(Origin - 1, NextOrig, Acc))
    end, Crates, Procedure),
    erlang:list_to_binary(lists:map(fun([H|_]) -> H end, array:to_list(FinalState))).

part1(Input) ->
    {Crates, Procedure} = parse_input(Input),
    apply_procedure(Crates, Procedure, fun(Cargo, Dest) ->
        lists:append(lists:reverse(Cargo), Dest)
    end).

part2(Input) ->
    {Crates, Procedure} = parse_input(Input),
    apply_procedure(Crates, Procedure, fun(Cargo, Dest) ->
        lists:append(Cargo, Dest)
    end).
