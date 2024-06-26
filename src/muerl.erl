-module(muerl).

-export([mutate/3]).

-type read_files_in_parallel() :: boolean().

%% @doc Runs a list of rules over a list of files and returns all the
%%      dead code pieces it can find.
-spec mutate([file:filename()], [mutator:t()], read_files_in_parallel()) ->
                #{results := [mutator:result()],
                  stats :=
                      #{parsing := Ms :: non_neg_integer(),
                        analyzing => Ms :: non_neg_integer(),
                        total => Ms :: non_neg_integer()}}.
mutate(Files, Mutators, ReadFilesInParallel) ->
    StartMs = erlang:monotonic_time(millisecond),
    {ParsingNanos, ASTs} = timer:tc(fun() -> get_asts(Files, ReadFilesInParallel) end),
    FilesAndASTs = lists:zip(Files, ASTs),
    erlang:yield(),
    {AnalyzingNanos, Results} = timer:tc(fun() -> mutate(Mutators, FilesAndASTs) end),
    TotalMs = erlang:monotonic_time(millisecond) - StartMs,
    #{results => Results,
      stats =>
          #{parsing => ParsingNanos div 1000,
            analyzing => AnalyzingNanos div 1000,
            total => TotalMs}}.

-spec mutate([mutator:t()], mutator:files_and_asts()) -> [mutator:result()].
mutate(Rules, ASTs) ->
    [Result || Rule <- Rules, Result <- mutator:analyze(Rule, ASTs)].

-spec get_asts([file:filename()], read_files_in_parallel()) -> [erl_syntax:forms()].
get_asts(Files, true) ->
    rpc:pmap({?MODULE, get_ast}, [], Files);
get_asts(Files, false) ->
    lists:map(fun get_ast/1, Files).

%% @hidden Only used through rpc:pmap/3
-spec get_ast(file:filename()) -> erl_syntax:forms().
get_ast(File) ->
    case ktn_dodger:parse_file(File, [no_fail, parse_macro_definitions]) of
        {ok, AST} ->
            AST;
        {error, OpenError} ->
            erlang:error({cant_parse, File, OpenError})
    end.
