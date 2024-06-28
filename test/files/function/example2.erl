-module(example2).

-export([a/1]).


a(X) when is_float(X) -> float;
a(X) when is_integer(X) -> integer;
a(X) when is_float(X) orelse is_integer(X) -> number;
a(X) when X > 1 -> gt_one;
a(_) -> whatever.