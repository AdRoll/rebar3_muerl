-module(example1).

-export([a/0]).

a() ->
    X = 1,
    case X of
        1 ->
            true;
        2 ->
            false
    end.
