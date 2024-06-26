-module(case_of).

-behaviour(mutator).

-export([mutate/1]).

mutate(FilesAndASTs) ->
    CaseOfs = case_of_usage(FilesAndASTs),
    % [{_, AST}] = FilesAndASTs,
    % io:format("erl_syntax:type/1: ~p~n", [AST]),

    % [X] = maps:values(CaseOfs),
    % io:format("~nCaseOfs=~p~n", [maps:values(CaseOfs)]),
    % lists:map(fun(X) -> io:format("~nCaseOfs=~p~n", [erl_prettypr:format(X)]) end, X),
    % [io:format("erl_syntax:type/1: ~p~n", [erl_syntax:type(Node)])
    %  || {_File, AST} <- FilesAndASTs,
    %     Node <- AST],
    [].

% case_of_usage(FilesAndASTs) ->
%     lists:foldl(fun({File, AST}, Result) ->
%                    FoldFun =
%                        fun(Node, FileCaseOfs) ->
%                           case erl_syntax:type(Node) == case_expr of
%                               true ->
%                                   io:format("case_expr found~ncase_expr_argument=~p~ncase_expr_clauses=~p~n",
%                                             [erl_syntax:case_expr_argument(Node), erl_syntax:case_expr_clauses(Node)]),
%                                   Node1 = erl_syntax:case_expr(erl_syntax:case_expr_argument(Node),
%                                                                lists:reverse(erl_syntax:case_expr_clauses(Node))),
%                                   io:format("Node(Def): ~p~nNode(Bef): ~p~nNode(Aft): ~p~n",
%                                             [Node, erl_prettypr:format(Node), erl_prettypr:format(Node1)]),
%                                 %   FileCaseOfs ++ Node;
%                                 % FileCaseOfs;
%                                 [Node | FileCaseOfs];
%                               _ ->
%                                 % io:format("AAAAAAAAA=~p~n", [erl_syntax:type(Node)]),
%                                 FileCaseOfs
%                           end
%                        end,
%                    ResultsForFile =
%                        try
%                            erl_syntax_lib:fold(FoldFun, [], erl_syntax:form_list(AST))
%                        catch
%                            syntax_error ->
%                                syntax_error
%                        end,
%                    maps:put(File, ResultsForFile, Result)
%                 end,
%                 #{},
%                 FilesAndASTs).
case_of_usage(FilesAndASTs) ->
    lists:foldl(fun({File, AST}, Result) ->
                    io:format("~nAST0=~p~n~n", [AST]),
                   FoldFun =
                       fun(Node) ->
                          case erl_syntax:type(Node) == case_expr of
                              true ->
                                Node1 = erl_syntax:case_expr(erl_syntax:case_expr_argument(Node),
                                                             lists:reverse(erl_syntax:case_expr_clauses(Node))),
                                %   FileCaseOfs ++ Node;
                                % FileCaseOfs;
                                Node1;
                              _ ->
                                % io:format("AAAAAAAAA=~p~n", [erl_syntax:type(Node)]),
                                Node
                          end
                       end,
                %    erl_syntax:update_tree(Node, Groups) <---------------
                   ResultsForFile =
                    [erl_syntax_lib:map(FoldFun, A) || A <- AST],
                    %    try
                    %        [erl_syntax_lib:map(FoldFun, A) || A <- AST]
                    %    catch
                    %        syntax_error ->
                    %            syntax_error
                    %    end,
                       io:format("~nAST1=~p~n", [ResultsForFile]),
                        PrettyPrinted = [erl_prettypr:format(R) || R <- ResultsForFile],
                    io:format("~n[File ~p]~n~ts~n~n", [File, PrettyPrinted]),
                    Abs = erl_parse:abstract(PrettyPrinted),
                    io:format("Abs: ~p~n~n", [Abs]),
                    try
                        B = erl_lint:module(Abs),
                        io:format("Valid? ~p~n", [B])
                    catch
                        _ ->
                            io:format("Invalid")
                    end,
                   maps:put(File, ResultsForFile, Result)
                end,
                #{},
                FilesAndASTs).