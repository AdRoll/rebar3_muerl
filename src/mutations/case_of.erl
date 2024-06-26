-module(case_of).

-export([mutate/1]).

mutate(FilesAndASTs) ->
    io:format("Got FilesAndASTs: ~p~n", [FilesAndASTs]),
    [].
