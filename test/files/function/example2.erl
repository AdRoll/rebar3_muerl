-module(example2).

-export([a/1, b/1, c/1]).

a(X) when is_float(X) ->
    float;
a(X) when is_integer(X) ->
    integer;
a(X) when is_float(X) orelse is_integer(X) ->
    float_or_integer;
a(X) when X > 1 ->
    gt_one;
a(_) ->
    whatever.

b(X)
    when is_atom(X)
         orelse is_binary(X)
         orelse is_bitstring(X)
         orelse is_boolean(X)
         orelse is_float(X)
         orelse is_function(X)
         orelse is_function(X, 1)
         orelse is_integer(X)
         orelse is_list(X)
         orelse is_map(X)
         orelse is_map_key(key, X)
         orelse is_number(X)
         orelse is_pid(X)
         orelse is_port(X)
         orelse is_tuple(X) ->
    many_guards;
b(_) ->
    whatever.

c(X) when is_tuple(X) ->
    tuple;
c(_) ->
    whatever.
