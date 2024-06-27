-module(muerl_utils).

-compile([export_all, nowarn_export_all]).

%% @doc Macro dodging version of erl_syntax:attribute_name/1
-spec attr_name(erl_syntax:syntaxTree()) -> atom().
attr_name(Node) ->
    N = erl_syntax:attribute_name(Node),
    try
        erl_syntax:concrete(N)
    catch
        _:_ ->
            N
    end.

%% @doc Whether the given Node node
%%      has defined the given AttrNames attribute names or not
-spec node_has_attrs(erl_syntax:syntaxTree(), atom() | [atom()]) -> boolean().
node_has_attrs(Node, AttrName) when not is_list(AttrName) ->
    node_has_attrs(Node, [AttrName]);
node_has_attrs(Node, AttrNames) ->
    erl_syntax:type(Node) == attribute andalso lists:member(attr_name(Node), AttrNames).

-spec get_pos_from_node(erl_syntax:syntaxTree()) -> erl_syntax:annotation_or_location().
get_pos_from_node(Node) ->
    case erl_syntax:get_pos(Node) of
        {L, C} ->
            {L, C};
        L when is_integer(L) ->
            L;
        Anno ->
            {erl_anno:line(Anno), erl_anno:column(Anno)}
    end.

-spec build_result(file:filename(), erl_syntax:syntaxTree()) -> muerl:result().
build_result(File, Node) ->
    Line =
        case get_pos_from_node(Node) of
            {L, _C} ->
                L;
            L ->
                L
        end,
    #{file => File,
      output => Node,
      line => Line}.
