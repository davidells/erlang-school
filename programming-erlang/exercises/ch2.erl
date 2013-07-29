-module(ch2).
-export([b_and/2, b_or/2, b_xor/2, b_not/1, b_nand/2]).

% 2.3

b_and(true, true) -> true;
b_and(_, _)       -> false.

b_or(true, _) -> true;
b_or(_, true) -> true;
b_or(_, _)    -> false.

b_xor(true, false) -> true;
b_xor(false, true) -> true;
b_xor(_, _)        -> false.

b_not(true)  -> false;
b_not(false) -> true.

b_nand(A, B) ->
    b_not(b_and(A, B)).
