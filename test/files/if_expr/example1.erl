-module(example1).

-export([a/1]).

a(X) ->
    if X < 5 ->
           lt;
       X > 5 ->
           gt;
       X == 5 ->
           eq;
       true ->
           wow
    end.
