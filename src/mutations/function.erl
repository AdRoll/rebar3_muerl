-module(function).

-behaviour(mutator).

-export([list_mutations/2]).
-export([get_mutation_string/2, get_mutation_print_format/2]).

-spec list_mutations(mutator:files_and_asts(), muerl:options()) -> [mutator:result()].
list_mutations(FilesAndASTs, Options) ->
    Functions = list_case_exprs(FilesAndASTs, Options),
    lists:flatten(Functions).

list_case_exprs(FilesAndASTs, Options) ->
    lists:foldl(fun({File, AST}, Result) ->
                   FoldFun =
                       fun(Node, Acc) ->
                          case erl_syntax:type(Node) == function of
                              true ->
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

get_mutation_string(Node, true = _PrettyPrint) ->
    erl_prettypr:format(Node);
get_mutation_string(Node, false) ->
    FunctionName =
        erl_prettypr:format(
            erl_syntax:function_name(Node)),
    FunctionArity = integer_to_list(erl_syntax:function_arity(Node)),
    string:join([FunctionName, FunctionArity], "/").

    % %% first, we remove the \n and a number of whitespaces that go afterward
    % Function0 =
    %     re:replace(
    %         erl_prettypr:format(Node), "\n(    )?", " ", [{return, list}, global]),
    % %% then, we remove everything after the first arm to avoid printing the whole thing
    % %% unless asked to (that's what `pretty_print_list` is for)
    % re:replace(Function0, ";.*end$", "; [...] end", [{return, list}, global]).

get_mutation_print_format(line_and_column, true = _PrettyPrint) ->
    "[mutator: ~p] ~ts:~p:~p: alter order of clauses of function~n```~n~ts~n```";
get_mutation_print_format(line, true) ->
    "[mutator: ~p] ~ts:~p: alter order of clauses of function~n```~n~ts~n```";
get_mutation_print_format(line_and_column, false) ->
    "[mutator: ~p] ~ts:~p:~p: alter order of clauses of function '~ts'";
get_mutation_print_format(line, false) ->
    "[mutator: ~p] ~ts:~p: alter order of clauses of function '~ts'".
