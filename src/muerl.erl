-module(muerl).

-export([run/2, run/3]).
%% Exported so rpc:pmap/3 works
-export([get_ast/1]).

-type options() :: #{parallel_processing => boolean(), pretty_print_list => boolean()}.
-type result() ::
    #{results := [mutator:result()],
      stats :=
          #{parsing := Ms :: non_neg_integer(),
            analyzing => Ms :: non_neg_integer(),
            total => Ms :: non_neg_integer()}}.

-export_type([result/0, options/0]).

-spec run([file:filename()], options()) -> result().
run(Files, Options) ->
    run(Files, mutator:default_mutators(), Options).

-spec run([file:filename()], [mutator:t()], options()) -> result().
run(Files, Mutators, Options) ->
    StartMs = erlang:monotonic_time(millisecond),
    ParallelProcessing = maps:get(parallel_processing, Options),
    Mode = maps:get(mode, Options),
    {ParsingNanos, ASTs} = timer:tc(fun() -> get_asts(Files, ParallelProcessing) end),
    FilesAndASTs = lists:zip(Files, ASTs),
    erlang:yield(),
    {RunningNanos, Results} =
        timer:tc(fun() ->
                    [Result
                     || Mutator <- Mutators, Result <- mutator:Mode(Mutator, FilesAndASTs, Options)]
                 end),
    TotalMs = erlang:monotonic_time(millisecond) - StartMs,
    #{results => Results,
      stats =>
          #{parsing => ParsingNanos div 1000,
            executing_mode => RunningNanos div 1000,
            total => TotalMs}}.

-spec get_asts([file:filename()], options()) -> [erl_syntax:forms()].
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
