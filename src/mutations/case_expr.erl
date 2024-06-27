-module(case_expr).

-behaviour(mutator).

-export([list_mutations/2]).

-spec list_mutations(mutator:files_and_asts(), muerl:options()) -> [mutator:result()].
list_mutations(FilesAndASTs, Options) ->
    CaseExprs = list_case_exprs(FilesAndASTs, Options),
    lists:flatten(CaseExprs).

list_case_exprs(FilesAndASTs, Options) ->
    lists:foldl(fun({File, AST}, Result) ->
                   FoldFun =
                       fun(Node, Acc) ->
                          case erl_syntax:type(Node) == case_expr of
                              true ->
                                  print_mutation(File, Node, Options),
                                  [Node | Acc];
                              _ ->
                                  Acc
                          end
                       end,
                   ResultsForFile =
                       [build_result(File, Match)
                        || Node <- AST,
                           Match <- erl_syntax_lib:fold(FoldFun, [], Node),
                           Match /= []],
                   [ResultsForFile | Result]
                end,
                [],
                FilesAndASTs).

build_result(File, Node) ->
    Line =
        case muerl_utils:get_pos_from_node(Node) of
            {L, _C} ->
                L;
            L ->
                L
        end,
    #{file => File,
      output => Node,
      line => Line}.

print_mutation(File, Node, #{pretty_print_list := true}) ->
    CaseExpr = erl_prettypr:format(Node),
    case muerl_utils:get_pos_from_node(Node) of
        {Line, Column} ->
            rebar_api:info("[mutator: ~p] ~ts:~p:~p: alter clauses order in~n```~n~ts~n```",
                           [?MODULE, File, Line, Column, CaseExpr]);
        Line ->
            rebar_api:info("[mutator: ~p] ~ts:~p: alter clauses order in~n```~n~ts~n```",
                           [?MODULE, File, Line, CaseExpr])
    end;
print_mutation(File, Node, _Options) ->
    [CaseExprArgument | _] =
        string:split(
            erl_prettypr:format(Node), "\n"),
    case muerl_utils:get_pos_from_node(Node) of
        {Line, Column} ->
            rebar_api:info("[mutator: ~p] ~ts:~p:~p: alter clauses order in '~ts'",
                           [?MODULE, File, Line, Column, CaseExprArgument]);
        Line ->
            rebar_api:info("[mutator: ~p] ~ts:~p: alter clauses order in '~ts'",
                           [?MODULE, File, Line, CaseExprArgument])
    end.
