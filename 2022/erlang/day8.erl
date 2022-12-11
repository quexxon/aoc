-module(day8).
-export([main/1]).

main([Input]) ->
    util:exec(Input, fun part1/1, fun part2/1).

parse_input(Input) ->
    array:from_list(lists:map(fun(Line) ->
        array:from_list(lists:map(fun(Char) ->
            {N, []} = string:to_integer(Char),
            N
        end, [[X] || <<X>> <= Line]))
    end, string:split(string:trim(Input), "\n", all))).

is_visible({OriginX, OriginY}, {XMax, YMax}, Grid) ->
    Height = array:get(OriginX, array:get(OriginY, Grid)),
    is_visible(
        {OriginX,OriginY-1}, Height, Grid,
        fun({_,Y}) -> Y =:= 0 end,
        fun({X, Y}) -> {X,Y-1} end
    ) orelse is_visible(
        {OriginX+1,OriginY}, Height, Grid,
        fun({X,_}) -> X =:= XMax end, 
        fun({X, Y}) -> {X+1,Y} end
    ) orelse is_visible(
        {OriginX, OriginY+1}, Height, Grid, 
        fun({_,Y}) -> Y =:= YMax end, 
        fun({X, Y}) -> {X,Y+1} end
    ) orelse is_visible(
        {OriginX-1, OriginY}, Height, Grid, 
        fun({X,_}) -> X =:= 0 end, 
        fun({X, Y}) -> {X-1,Y} end
    ).

is_visible({X,Y}=Point, Height, Grid, Is_Edge, Next_Point) ->
    case array:get(X, array:get(Y, Grid)) < Height of
        true ->
            Is_Edge(Point) orelse
            is_visible(Next_Point(Point), Height, Grid, Is_Edge, Next_Point);
        false -> false
    end.


viewing_distance({OriginX,OriginY}, {XMax,YMax}, Grid) ->
    case OriginX =:= 0 orelse OriginY =:= 0 orelse OriginX =:= XMax orelse OriginY =:= YMax of
        true -> 0;
        false ->
            Height = array:get(OriginX, array:get(OriginY, Grid)),
            viewing_distance(
                0, {OriginX,OriginY-1}, Height, Grid,
                fun({_,Y}) -> Y =:= 0 end,
                fun({X, Y}) -> {X,Y-1} end
            ) * viewing_distance(
                0, {OriginX+1,OriginY}, Height, Grid,
                fun({X,_}) -> X =:= XMax end, 
                fun({X, Y}) -> {X+1,Y} end
            ) * viewing_distance(
                0, {OriginX, OriginY+1}, Height, Grid, 
                fun({_,Y}) -> Y =:= YMax end, 
                fun({X, Y}) -> {X,Y+1} end
            ) * viewing_distance(
                0, {OriginX-1, OriginY}, Height, Grid, 
                fun({X,_}) -> X =:= 0 end, 
                fun({X, Y}) -> {X-1,Y} end
            )
    end.

viewing_distance(Count, {X,Y}=Point, Height, Grid, Is_Edge, Next_Point) ->
    case array:get(X, array:get(Y, Grid)) < Height of
        true -> 
            case Is_Edge(Point) of
                true -> Count + 1;
                false ->
                    viewing_distance(
                        Count + 1, Next_Point(Point), Height, Grid, Is_Edge, Next_Point
                    )
            end;
        false -> Count + 1
    end.

part1(Input) ->
    Grid = parse_input(Input),
    Width = array:size(array:get(0, Grid)),
    Height = array:size(Grid),
    Limits = { Width - 1, Height - 1 },
    Points = [{X, Y} || X <- lists:seq(1, Width - 2), Y <- lists:seq(1, Height - 2)],
    InnerVisible = [Point || Point <- Points, is_visible(Point, Limits, Grid)],
    length(InnerVisible) + (Width * 2) + (Height * 2) - 4.

part2(Input) ->
    Grid = parse_input(Input),
    Width = array:size(array:get(0, Grid)),
    Height = array:size(Grid),
    Limits = { Width - 1, Height - 1 },
    ViewingDistances = [ viewing_distance({X, Y}, Limits, Grid) ||
        X <- lists:seq(0, Width - 1), Y <- lists:seq(0, Height - 1)],
    lists:max(ViewingDistances).
