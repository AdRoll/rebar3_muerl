-module(example1).

-export([a/1, b/2]).

a(1) -> one;
a([]) -> empty_list;
a(X) when X > 1 -> gt_one;
a(_) -> whatever.


b(true, true) -> true;
b(true, false) -> false;
b(false, true) -> false;
b(false, false) -> false;
b(X, Y) -> whatever.