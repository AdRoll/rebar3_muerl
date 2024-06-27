-module(case_of).

-behaviour(mutator).

-export([list_mutations/1]).

-spec list_mutations(mutator:files_and_asts()) -> [mutator:result()].
list_mutations(FilesAndASTs) ->
    CaseOfs = list_case_ofs(FilesAndASTs),
    lists:flatten(CaseOfs).

list_case_ofs(FilesAndASTs) ->
    lists:foldl(fun({File, AST}, Result) ->
                   FoldFun =
                       fun(Node, Acc) ->
                          case erl_syntax:type(Node) == case_expr of
                              true ->
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
        case erl_syntax:get_pos(Node) of
            {L, _C} ->
                L;
            L when is_integer(L) ->
                L;
            Anno ->
                erl_anno:line(Anno)
        end,
    #{file => File,
      output => Node,
      line => Line}.
