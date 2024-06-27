-module(example1).

-export([a/1]).

a(X) ->
    receive
        {spiderman, peter_parker} ->
            omg;
        42 ->
            42;
        N when is_integer(N) ->
            a_number;
        X ->
            match;
        true ->
            wow
    end.