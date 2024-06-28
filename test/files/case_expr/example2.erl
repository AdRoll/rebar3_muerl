-module(example2).

-export([a/0, b/1]).

a() ->
    X = 1,
    case X of
        1 ->
            true;
        2 ->
            false
    end.

b(Z) ->
    case Z of
        {ok, 1} ->
            one;
        two ->
            two;
        _ ->
            three
    end.
