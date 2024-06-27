%%% @doc Behavior for defining a mutation.
-module(mutator).

-type t() :: module().
-type files_and_asts() :: [{file:filename(), erl_syntax:forms()}].
-type result() ::
    #{file := file:filename(),
      line := non_neg_integer(),
      output := term(),
      mutator => t()}.

-export_type([t/0, files_and_asts/0, result/0]).

-callback mutate(files_and_asts()) -> [result()].
-callback list_mutations(files_and_asts(), muerl:options()) -> [result()].

%% Remove when `mutate` gets implemented
-optional_callbacks([mutate/1]).

-export([mutate/3, list_mutations/3, default_mutators/0]).

%% @doc Apply the mutations from the mutator to the given files.
-spec mutate(t(), files_and_asts(), muerl:options()) -> [result()].
mutate(Mutator, FilesAndASTs, _Options) ->
    try
        [Result#{mutator => Mutator} || Result <- Mutator:mutate(FilesAndASTs)]
    catch
        _:Error:Stack ->
            logger:error("~p:mutate/2 failed with Error ~p \nStack: ~p", [Mutator, Error, Stack]),
            erlang:error(mutate_error)
    end.

%% @doc Find the mutations from the mutator in the given files.
-spec list_mutations(t(), files_and_asts(), muerl:options()) -> [result()].
list_mutations(Mutator, FilesAndASTs, Options) ->
    try
        [Result#{mutator => Mutator} || Result <- Mutator:list_mutations(FilesAndASTs, Options)]
    catch
        _:Error:Stack ->
            logger:error("~p:list_mutations/2 failed with Error ~p \nStack: ~p",
                         [Mutator, Error, Stack]),
            erlang:error(mutate_error)
    end.

%% @doc The list of default mutators to generate and apply.
-spec default_mutators() -> [].
default_mutators() ->
    [Module
     || File
            <- filelib:wildcard(
                   filename:join([code:lib_dir(rebar3_muerl), "**/*.beam"])),
        Module <- [list_to_atom(filename:basename(File, ".beam"))],
        {behaviour, Behaviours} <- Module:module_info(attributes),
        lists:member(?MODULE, Behaviours)].
