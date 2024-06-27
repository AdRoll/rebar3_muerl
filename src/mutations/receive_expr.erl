-module(receive_expr).

-behaviour(mutator).

-export([list_mutations/2]).

-spec list_mutations(mutator:files_and_asts(), muerl:options()) -> [mutator:result()].
list_mutations(FilesAndASTs, Options) ->
    ReceiveExprs = list_receive_exprs(FilesAndASTs, Options),
    lists:flatten(ReceiveExprs).

list_receive_exprs(FilesAndASTs, Options) ->
    lists:foldl(fun({File, AST}, Result) ->
                   FoldFun =
                       fun(Node, Acc) ->
                          case erl_syntax:type(Node) == receive_expr of
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
    ReceiveExpr = erl_prettypr:format(Node),
    case muerl_utils:get_pos_from_node(Node) of
        {Line, Column} ->
            rebar_api:info("[mutator: ~p] ~ts:~p:~p: alter order of clauses in~n```~n~ts~n```",
                           [?MODULE, File, Line, Column, ReceiveExpr]);
        Line ->
            rebar_api:info("[mutator: ~p] ~ts:~p: alter order of clauses in~n```~n~ts~n```",
                           [?MODULE, File, Line, ReceiveExpr])
    end;
print_mutation(File, Node, _Options) ->
    %% first, we remove the \n and a number of whitespaces that go afterward
    ReceiveExpr0 =
        re:replace(
            erl_prettypr:format(Node), "\n(    )?", " ", [{return, list}, global]),
    %% then, we remove everything after the first arm to avoid printing the whole thing
    %% unless asked to (that's what `pretty_print_list` is for)
    ReceiveExpr = re:replace(ReceiveExpr0, ";.*end$", "; [...] end", [{return, list}, global]),
    case muerl_utils:get_pos_from_node(Node) of
        {Line, Column} ->
            rebar_api:info("[mutator: ~p] ~ts:~p:~p: alter order of clauses in '~ts'",
                           [?MODULE, File, Line, Column, ReceiveExpr]);
        Line ->
            rebar_api:info("[mutator: ~p] ~ts:~p: alter order of clauses in '~ts'",
                           [?MODULE, File, Line, ReceiveExpr])
    end.
