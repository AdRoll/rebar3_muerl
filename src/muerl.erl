-module(muerl).

-export([list_mutations/2, list_mutations/3]).

-type read_files_in_parallel() :: boolean().
-type result() ::
    #{results := [mutator:result()],
      stats :=
          #{parsing := Ms :: non_neg_integer(),
            analyzing => Ms :: non_neg_integer(),
            total => Ms :: non_neg_integer()}}.

-spec list_mutations([file:filename()], read_files_in_parallel()) -> result().
list_mutations(Files, ReadFilesInParallel) ->
    list_mutations(Files, mutator:default_mutators(), ReadFilesInParallel).

%% @doc Runs a list of mutators over a list of files and returns all the
%%      mutations that could be applied in the files.
-spec list_mutations([file:filename()], [mutator:t()], read_files_in_parallel()) ->
                        result().
list_mutations(Files, Mutators, ReadFilesInParallel) ->
    StartMs = erlang:monotonic_time(millisecond),
    {ParsingNanos, ASTs} = timer:tc(fun() -> get_asts(Files, ReadFilesInParallel) end),
    FilesAndASTs = lists:zip(Files, ASTs),
    erlang:yield(),
    {AnalyzingNanos, Results} =
        timer:tc(fun() ->
                    [Result
                     || Mutator <- Mutators,
                        Result <- mutator:list_mutations(Mutator, FilesAndASTs)]
                 end),
    TotalMs = erlang:monotonic_time(millisecond) - StartMs,
    #{results => Results,
      stats =>
          #{parsing => ParsingNanos div 1000,
            analyzing => AnalyzingNanos div 1000,
            total => TotalMs}}.

-spec get_asts([file:filename()], read_files_in_parallel()) -> [erl_syntax:forms()].
get_asts(Files, true) ->
    rpc:pmap({?MODULE, get_ast}, [], Files);
get_asts(Files, false) ->
    lists:map(fun get_ast/1, Files).

%% @hidden Only used through rpc:pmap/3
-spec get_ast(file:filename()) -> erl_syntax:forms().
get_ast(File) ->
    case epp:parse_file(File, [no_fail, parse_macro_definitions, {location, {1, 1}}]) of
        {ok, AST} ->
            AST;
        {error, OpenError} ->
            erlang:error({cant_parse, File, OpenError})
    end.
