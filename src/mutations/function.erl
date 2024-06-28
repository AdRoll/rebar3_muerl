-module(function).

-behaviour(mutator).

-export([list_mutations/2]).
-export([get_mutation_string/2, get_mutation_print_format/2]).

-spec list_mutations(mutator:files_and_asts(), muerl:options()) -> [mutator:result()].
list_mutations(FilesAndASTs, Options) ->
    Functions = list_function_exprs(FilesAndASTs, Options),
    lists:flatten(Functions).

list_function_exprs(FilesAndASTs, Options) ->
    lists:foldl(fun({File, AST}, Result) ->
                   FoldFun =
                       fun(Node, Acc) ->
                          case erl_syntax:type(Node) == function of
                              true ->
                                  FunctionGuards =
                                      lists:filter(fun(Clause) ->
                                                      erl_syntax:clause_guard(Clause) /= none
                                                   end,
                                                   erl_syntax:function_clauses(Node)),
                                  case FunctionGuards of
                                      [] ->
                                          ok;
                                      _ ->
                                          mutator:print_mutation(?MODULE,
                                                                 File,
                                                                 Node,
                                                                 Options#{function_guards =>
                                                                              FunctionGuards})
                                  end,
                                  mutator:print_mutation(?MODULE, File, Node, Options),
                                  [Node | Acc];
                              _ ->
                                  Acc
                          end
                       end,
                   ResultsForFile =
                       [muerl_utils:build_result(File, Match)
                        || Node <- AST,
                           Match <- erl_syntax_lib:fold(FoldFun, [], Node),
                           Match /= []],
                   [ResultsForFile | Result]
                end,
                [],
                FilesAndASTs).

get_mutation_string(Node, #{pretty_print := true}) ->
    erl_prettypr:format(Node);
%% TODO: print these properly
get_mutation_string(Node,
                    #{pretty_print := false, function_guards := _FunctionGuardsAST}) ->
    % FunctionName =
    %     erl_prettypr:format(
    %         erl_syntax:function_name(Node)),
    % FunctionGuards = lists:map(fun erl_prettypr:format/1, FunctionGuardsAST),
    % case FunctionGuards of
    %     %% at least two guards
    %     [_, _ | _] ->
    %         string:join([FunctionName, hd(FunctionGuards)], "");
    %     _ ->
    %         string:join([FunctionName, hd(FunctionGuards)], "")
    % end;
    get_mutation_string(Node, #{pretty_print => false});
get_mutation_string(Node, #{pretty_print := false}) ->
    FunctionName =
        erl_prettypr:format(
            erl_syntax:function_name(Node)),
    FunctionArity = integer_to_list(erl_syntax:function_arity(Node)),
    string:join([FunctionName, FunctionArity], "/").

%% when it DOES have guards in the function definition
get_mutation_print_format(line_and_column,
                          #{pretty_print := true, function_guards := _}) ->
    "[mutator: ~p] ~ts:~p:~p: alter order of guards (or remove them) in function~n```~n~ts~n```";
get_mutation_print_format(line, #{pretty_print := true, function_guards := _}) ->
    "[mutator: ~p] ~ts:~p: alter order of guards (or remove them) in function~n```~n~ts~n```";
get_mutation_print_format(line_and_column,
                          #{pretty_print := false, function_guards := _}) ->
    "[mutator: ~p] ~ts:~p:~p: alter order of guards (or remove them) in function '~ts'";
get_mutation_print_format(line, #{pretty_print := false, function_guards := _}) ->
    "[mutator: ~p] ~ts:~p: alter order of guards (or remove them) in function '~ts'";
%% when it does not have guards in the function definition
get_mutation_print_format(line_and_column, #{pretty_print := true}) ->
    "[mutator: ~p] ~ts:~p:~p: alter order of clauses in function~n```~n~ts~n```";
get_mutation_print_format(line, #{pretty_print := true}) ->
    "[mutator: ~p] ~ts:~p: alter order of clauses in function~n```~n~ts~n```";
get_mutation_print_format(line_and_column, #{pretty_print := false}) ->
    "[mutator: ~p] ~ts:~p:~p: alter order of clauses in function '~ts'";
get_mutation_print_format(line, #{pretty_print := false}) ->
    "[mutator: ~p] ~ts:~p: alter order of clauses in function '~ts'".
