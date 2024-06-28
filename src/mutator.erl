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

%% Callbacks for the two running modes
-callback mutate(files_and_asts()) -> [result()].
-callback list_mutations(files_and_asts(), muerl:options()) -> [result()].
%% Callbacks to DRY the printing of the mutations
-callback get_mutation_string(erl_syntax:syntaxTree(), boolean()) -> string().
-callback get_mutation_print_format(line_and_column | line, boolean()) -> io:format().

%% Remove when `mutate` gets implemented
-optional_callbacks([mutate/1]).

-export([mutate/3, list_mutations/3, default_mutators/0, print_mutation/4]).

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

-spec print_mutation(t(), file:filename(), erl_syntax:syntaxTree(), muerl:options()) ->
                        ok.
print_mutation(Mutator, File, Node, Options) ->
    PrettyPrint = maps:get(pretty_print_list, Options),
    Expr = Mutator:get_mutation_string(Node, PrettyPrint),
    case muerl_utils:get_pos_from_node(Node) of
        {Line, Column} ->
            rebar_api:info(
                Mutator:get_mutation_print_format(line_and_column, PrettyPrint),
                [Mutator, File, Line, Column, Expr]);
        Line ->
            rebar_api:info(
                Mutator:get_mutation_print_format(line, PrettyPrint), [Mutator, File, Line, Expr])
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
