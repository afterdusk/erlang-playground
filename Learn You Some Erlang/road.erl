-module(road).

-compile(export_all).

%% Heathrow to London

%% my implementation
main() ->
    File = "road.txt",
    {ok, Bin} = file:read_file(File),
    Map = parse_map(Bin),
    best_path(Map).

%% Transform a string into a readable map of triples
parse_map(Bin) when is_binary(Bin) ->
    parse_map(binary_to_list(Bin));
parse_map(Str) when is_list(Str) ->
    Values = [list_to_integer(X) || X <- string:tokens(Str, "\r\n\t ")],
    group_vals(Values, []).

%% review: write this tail recursively in future
group([A, B, X | List]) ->
    [{A, B, X} | group(List)];
group([]) ->
    [].

best_path(Map) ->
    min(best_path(Map, a), best_path(Map, b)).

%% review: lmao consider using folds, which are tail recursively
best_path([], _) ->
    {0, []};
best_path([{A, _, X} | Rest], a) ->
    {CostA, PathA} = best_path(Rest, a),
    {CostB, PathB} = best_path(Rest, b),
    UpdatedCostA = CostA + A,
    UpdatedCostB = CostB + A + X,
    if UpdatedCostA =< UpdatedCostB ->
           {UpdatedCostA, [{a, A} | PathA]};
       UpdatedCostA > UpdatedCostB ->
           {UpdatedCostB, [{a, A}, {x, X} | PathB]}
    end;
best_path([{_, B, X} | Rest], b) ->
    {CostA, PathA} = best_path(Rest, a),
    {CostB, PathB} = best_path(Rest, b),
    UpdatedCostA = CostA + B + X,
    UpdatedCostB = CostB + B,
    if UpdatedCostA =< UpdatedCostB ->
           {UpdatedCostA, [{b, B}, {x, X} | PathA]};
       UpdatedCostA > UpdatedCostB ->
           {UpdatedCostB, [{b, B} | PathB]}
    end.

%% LYSE implementantation
main([FileName]) ->
    {ok, Bin} = file:read_file(FileName),
    Map = parse_map(Bin),
    io:format("~p~n", [optimal_path(Map)]),
    erlang:halt(0).

%% Transform a string into a readable map of triples
parse_map(Bin) when is_binary(Bin) ->
    parse_map(binary_to_list(Bin));
parse_map(Str) when is_list(Str) ->
    Values = [list_to_integer(X) || X <- string:tokens(Str, "\r\n\t ")],
    group_vals(Values, []).

group_vals([], Acc) ->
    lists:reverse(Acc);
group_vals([A, B, X | Rest], Acc) ->
    group_vals(Rest, [{A, B, X} | Acc]).

%% Picks the best of all paths, woo!
optimal_path(Map) ->
    {A, B} = lists:foldl(fun shortest_step/2, {{0, []}, {0, []}}, Map),
    {_Dist, Path} =
        if hd(element(2, A)) =/= {x, 0} ->
               A;
           hd(element(2, B)) =/= {x, 0} ->
               B
        end,
    lists:reverse(Path).

%% actual problem solving
%% change triples of the form {A,B,X}
%% where A,B,X are distances and a,b,x are possible paths
%% to the form {DistanceSum, PathList}.
shortest_step({A, B, X}, {{DistA, PathA}, {DistB, PathB}}) ->
    OptA1 = {DistA + A, [{a, A} | PathA]},
    OptA2 = {DistB + B + X, [{x, X}, {b, B} | PathB]},
    OptB1 = {DistB + B, [{b, B} | PathB]},
    OptB2 = {DistA + A + X, [{x, X}, {a, A} | PathA]},
    {erlang:min(OptA1, OptA2), erlang:min(OptB1, OptB2)}.
