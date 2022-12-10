-module(day7).
-export([main/1]).

main([Input]) ->
    util:exec(Input, fun part1/1, fun part2/1).

parse_input(Input) ->
    parse_input(string:split(string:trim(Input), "\n", all), maps:new(), []).

parse_input([], Tree, _) -> Tree;
parse_input([Line|Lines], Tree, Cwd) ->
    case string:slice(Line, 0, 4) of
        <<"$ cd">> ->
            Path = binary:bin_to_list(string:slice(Line, 5)),
            Directory = case Path of
                "/" -> [];
                ".." -> tl(Cwd);
                _ -> [Path|Cwd]
            end,
            parse_input(Lines, Tree, Directory);
        <<"$ ls">> -> 
            parse_input(Lines, Tree, Cwd);
        _ ->
            [Front, Back] = string:split(Line, " "),
            Path = "/" ++ string:join(lists:reverse(Cwd), "/"),
            {Directories, Files} = maps:get(Path, Tree, {[], []}),
            Node = case Front of
                <<"dir">> -> {[binary:bin_to_list(Back)|Directories], Files};
                _ ->
                    Size = case string:to_integer(Front) of
                        {error, Reason} -> throw(Reason);
                        {Int, _} -> Int
                    end,
                    {Directories,[{Back, Size}|Files]}
            end,
            NextTree = maps:put(Path, Node, Tree),
            parse_input(Lines, NextTree, Cwd)
    end.

get_directory_sizes(Cwd, DirectorySizes, Tree) ->
    {Directories, Files} = maps:get(Cwd, Tree),
    FileSizeSum = lists:foldl(fun({_,Size}, Acc) -> Size + Acc end, 0, Files),
    { DirSize, DirSizes } = lists:foldl((fun (Dir, {TotalDirSize, DirSizes}) ->
        NextCwd = case Cwd of
            "/" -> Cwd ++ Dir;
            _ -> string:join([Cwd, Dir], "/")
        end,
        {DirSize, NextDirSizes} = get_directory_sizes(NextCwd, DirSizes, Tree),
        {DirSize + TotalDirSize, NextDirSizes}
    end), {FileSizeSum, DirectorySizes}, Directories),
    { DirSize, maps:put(Cwd, DirSize, DirSizes) }.

part1(Input) -> 
    Tree = parse_input(Input),
    {_, DirectorySizes} = get_directory_sizes("/", maps:new(), Tree),
    MaxSize = 100000,
    lists:foldr(fun(Size, Sum) ->
        if
            Size > MaxSize -> Sum;
            Size =< MaxSize -> Sum + Size
        end
    end, 0, maps:values(DirectorySizes)).

part2(Input) -> 
    DiskSpace = 70000000,
    DesiredFreeSpace = 30000000,
    Tree = parse_input(Input),
    {_, DirectorySizes} = get_directory_sizes("/", maps:new(), Tree),
    UsedSpace = maps:get("/", DirectorySizes),
    lists:min(lists:filter(
        fun(Size) -> DiskSpace - (UsedSpace - Size) >= DesiredFreeSpace end,
        maps:values(DirectorySizes)
    )).
