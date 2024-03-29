-module(fifo).

-export([new/0, push/2, pop/1, empty/1]).

%% implemented as two stacks; push on the first, pop on the second.
%% when the second is empty, reverse the first and make it the second.
new() ->
    {fifo, [], []}.

push({fifo, In, Out}, X) ->
    {fifo, [X | In], Out}.

pop({fifo, [], []}) ->
    erlang:error('empty fifo');
pop({fifo, In, []}) ->
    pop({fifo, [], lists:reverse(In)});
pop({fifo, In, [H | T]}) ->
    {H, {fifo, In, T}}.

empty({fifo, [], []}) ->
    true;
empty({fifo, _, _}) ->
    false.
