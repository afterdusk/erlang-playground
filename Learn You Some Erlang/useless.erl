-module(useless).

-compile(export_all).
-export([add/2, hello/0, greet_and_add_two/1]).

add(A, B) ->
  A + B.

hello() ->
  io:format("Hello, world! ~n").

greet_and_add_two(X) ->
  hello(),
  add(X,2).

print_test() ->
  io:format("~s~n", [<<"Hello">>]),
  io:format("~p~n", [<<"Hello">>]),
  io:format("~~~n"),
  io:format("~f~n", [4.0]),
  io:format("~30f~n", [4.0]).