-module(cases).

-export([beach/1, insert/2]).

insert(X, []) ->
    [X];
insert(X, Set) ->
    case lists:member(X, Set) of
        true ->
            Set;
        fasle ->
            [X | Set]
    end.

beach(Temperature) ->
    case Temperature of
        {celsius, N} when N >= 20, N =< 45 ->
            favourable;
        {kelvin, N} when N >= 293, N =< 318 ->
            'scientifically favorable';
        {fahrenheit, N} when N >= 68, N =< 113 ->
            'favorable in the US';
        _ ->
            'avoid beach'
    end.
