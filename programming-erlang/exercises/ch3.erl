-module(ch3).
-export([sum/1, sum/2, create/1, create_reverse/1, print/1, print_even/1, db_new/0, db_destroy/1, db_write/3, db_delete/2, db_read/2, db_match/2, filter/2, reverse/1, concatenate/1, flatten/1, flatten/2, quicksort/1, mergesort/1, db2_new/0, db2_destroy/1, db2_write/3, db2_delete/2, db2_read/2, db2_match/2, parse/1, eval/1, pretty_print/1, compile/1, simulate/1, index_listing/1]).

% 3.1

sum(N) when N < 0 -> erlang:error(badarg);
sum(0) -> 0;
sum(N) -> N + sum(N-1).

sum(N, M) when N > M -> 0;
sum(N, M) -> N + sum(N+1, M).


% 3.2

create(N) -> lists:reverse(create_reverse(N)).

create_reverse(N) when N < 0 -> erlang:error(badarg);
create_reverse(0) -> [];
create_reverse(N) -> [N | create_reverse(N-1)].


% 3.3

print(N) -> print(N, 1).

print(N, _) when N < 0 -> erlang:error(badarg);
print(N, Current) when Current > N -> ok;
print(N, Current) -> 
    io:format("Number:~p~n", [Current]),
    print(N, Current + 1).


print_even(N) -> print_even(N, 1).

print_even(N, _) when N < 0 -> erlang:error(badarg);
print_even(N, Current) when Current > N -> ok;
print_even(N, Current) when (Current rem 2) =:= 0 ->
    io:format("Number:~p~n", [Current]),
    print_even(N, Current + 1);
print_even(N, Current) ->
    print_even(N, Current + 1).



% 3.4

db_new() -> [].

db_destroy(_Db) -> ok.

db_write(Key, Element, Db) ->
    [{Key, Element} | Db].

db_delete(_, []) -> [];
db_delete(Key, [{Key, _}|Tail]) -> 
    db_delete(Key, Tail);
db_delete(Key, [Head|Tail]) ->
    [Head | db_delete(Key, Tail)].
    
db_read(_, []) -> {error, instance};
db_read(Key, [{Key, Element}|_]) -> 
    {ok, Element};
db_read(Key, [_|Tail]) -> 
    db_read(Key, Tail).

db_match(_, []) -> [];
db_match(Element, [{Key, Element}|Tail]) -> 
    [Key | db_match(Element, Tail)];
db_match(Element, [_|Tail]) ->
    db_match(Element, Tail).




% 3.5

filter(List, N) ->
    filter(List, N, []).

filter([], _N, Acc) -> 
    lists:reverse(Acc);
filter([Head|Tail], N, Acc) when Head =< N ->
    filter(Tail, N, [Head|Acc]);
filter([_Head|Tail], N, Acc) ->
    filter(Tail, N, Acc).


reverse(List) ->
    reverse(List, []).

reverse([], Acc) -> Acc;
reverse([Head|Tail], Acc) ->
    reverse(Tail, [Head|Acc]).


concatenate(ListOfLists) ->
    concatenate(ListOfLists, []).

concatenate([], Acc) -> 
    lists:reverse(Acc);
concatenate([List|Tail], Acc) ->
    concatenate(Tail, reverse(List, Acc)).


flatten(List) ->
    flatten(List, []).

flatten([], Acc) ->
    Acc;
flatten([Head|Tail], Acc) ->
    flatten(Head, flatten(Tail, Acc));
flatten(Element, Acc) ->
    [Element|Acc].



% 3.6

quicksort([]) ->
    [];
quicksort([Head|Tail]) ->
    Less = quicksort([X || X <- Tail, X < Head]),
    Rest = quicksort([X || X <- Tail, X >= Head]),
    concatenate([Less, [Head], Rest]).

mergesort([]) ->
    [];
mergesort([Single]) ->
    [Single];
mergesort(List) ->
    Length = length(List),
    HalfIndex = erlang:trunc(Length / 2),
    lists:merge(
      mergesort(lists:sublist(List, 1, HalfIndex)),
      mergesort(lists:sublist(List, HalfIndex + 1, Length - HalfIndex))).


% 3.7

db2_new() -> [].

db2_destroy(_Db) -> ok.

db2_write(Key, Element, Db) -> 
    [{Key, Element} | Db].

db2_delete(Key, List) ->
    lists:keydelete(Key, 1, List).
    
db2_read(Key, List) ->
    case lists:keyfind(Key, 1, List) of
        false -> {error, instance};
        {Key, Element} -> {ok, Element}
    end.
            
db2_match(Element, List) ->
    Filtered = lists:filter(fun({_K,V}) -> V =:= Element end, List),
    lists:map(fun({K, _V}) -> K end, Filtered).



% 3.8
%
% Grammar:
%
% Expr -> (Expr Op Expr) | UnaryOp Exp | Integer
% Op -> + | - | * | /
% UnaryOp -> ~

% --------------
% Parsing
% --------------

parse(Expr) ->
    {_ParsedLen, Parsed} = parse_r(Expr),
    Parsed.

parse_r([$(|Tail]) ->
    ClosingParenIndex = find_close_paren(Tail),
    {ExprALen, ExprA} = parse_r(Tail),
    Op = opstr_atom(string:substr(Tail, ExprALen + 1, 1)),
    ExprBStr = string:sub_string(Tail, ExprALen + 2, ClosingParenIndex),
    {_, ExprB} = parse_r(ExprBStr),
    { ClosingParenIndex + 1, { Op, ExprA, ExprB } };

parse_r([$~|Tail]) ->
    {_, Expr} = parse_r(Tail),
    { 1, { negate, Expr } };

parse_r(Number) ->
    EndingIndex = string:cspan(Number, "+-*/()~"),
    NumberStr = string:substr(Number, 1, EndingIndex),
    { EndingIndex, { num, list_to_integer(NumberStr) } }.


opstr_atom(OpStr) ->
    case OpStr of
        "+" -> plus;
        "-" -> minus;
        "*" -> times;
        "/" -> divide
    end.


find_close_paren(Str) ->
    find_close_paren(Str, 0, 1).

find_close_paren([], _Level, _Index) ->
    0;
find_close_paren([$)|_], 0, Index) ->
    Index;
find_close_paren([$(|Tail], Level, Index) ->
    find_close_paren(Tail, Level+1, Index+1);
find_close_paren([$)|Tail], Level, Index) ->
    find_close_paren(Tail, Level-1, Index+1);
find_close_paren([_|Tail], Level, Index) ->
    find_close_paren(Tail, Level, Index+1).


% --------------
% Eval
% --------------

eval({num, Num}) ->
    Num;
eval({plus, ExprA, ExprB}) ->
    eval(ExprA) + eval(ExprB);
eval({minus, ExprA, ExprB}) ->
    eval(ExprA) - eval(ExprB);
eval({times, ExprA, ExprB}) ->
    eval(ExprA) * eval(ExprB);
eval({divide, ExprA, ExprB}) ->
    eval(ExprA) / eval(ExprB);
eval({negate, Expr}) ->
    -1 * eval(Expr).


% --------------
% Print
% --------------

pretty_print({num, Num}) ->
    integer_to_list(Num);
pretty_print({negate, Expr}) ->
    "~" ++ pretty_print(Expr);
pretty_print({Op, ExprA, ExprB}) ->
    OpStr = case Op of 
                plus -> "+";
                minus -> "-";
                times -> "*";
                divide -> "/"
            end,
    "(" ++ pretty_print(ExprA) ++ OpStr 
        ++ pretty_print(ExprB) ++ ")".


% --------------
% Compile to stack machine
% --------------

compile(Expr) ->
    compile(Expr, []).

compile({num, Num}, Acc) ->
    [Num | Acc];
compile({negate, Expr}, Acc) ->
    compile(Expr) ++ [negate | Acc];
compile({Op, ExprA, ExprB}, Acc) ->
    compile(ExprB) ++ compile(ExprA) ++ [ Op | Acc ].


% --------------
% Simulate stack machine
% --------------

simulate(Compiled) ->
    simulate(Compiled, []).

simulate([], [Current]) ->
    Current;
simulate([negate | Instructions], [Current | Stack]) ->
    simulate(Instructions, [-1 * Current | Stack]);
simulate([plus | Instructions], [TermA, TermB | Stack]) ->
    simulate(Instructions, [TermA + TermB | Stack]);
simulate([minus | Instructions], [TermA, TermB | Stack]) ->
    simulate(Instructions, [TermA - TermB | Stack]);
simulate([times | Instructions], [TermA, TermB | Stack]) ->
    simulate(Instructions, [TermA * TermB| Stack]);
simulate([divide | Instructions], [TermA, TermB | Stack]) ->
    simulate(Instructions, [TermA / TermB | Stack]);
simulate([Number | Instructions], Stack) ->
    simulate(Instructions, [Number | Stack]).





% 3.9

index_listing([]) ->
    io:format("~n");

index_listing([{Keyword, OccurrenceList}|Tail]) ->
    IndexForm = index_form(OccurrenceList),
    io:format("~s\t\t~s~n", [Keyword, index_str(IndexForm)]),
    index_listing(Tail).


% Helper function: turns [1,1,2,4,5,6,8] -> [{1,2},{4,6},{8,8}]

index_form([]) -> [];

index_form([Head|Tail]) ->
    index_form(Tail, Head, Head, []).

index_form([], CurrentStart, CurrentEnd, Acc) ->
    lists:reverse([{CurrentStart, CurrentEnd} | Acc]);

index_form([Head|Tail], CurrentStart, CurrentEnd, Acc) 
  when Head =:= CurrentStart; 
       Head =:= CurrentEnd ->
    index_form(Tail, CurrentStart, CurrentEnd, Acc);

index_form([Head|Tail], CurrentStart, CurrentEnd, Acc) 
  when Head =:= (CurrentEnd + 1) ->
    index_form(Tail, CurrentStart, Head, Acc);

index_form([Head|Tail], CurrentStart, CurrentEnd, Acc) ->
    index_form(Tail, Head, Head, [{CurrentStart, CurrentEnd} | Acc]).


% Helper function: turns [{1,2},{4,6},{8,8}] -> "1-2,4-6,8"

index_str(IndexForm) ->
    index_str(lists:reverse(IndexForm), []).

index_str([], Acc) -> 
    Acc;

index_str([{Start,End}|Tail], Acc) ->
    RangeStr = 
        case Start =:= End of
            true -> integer_to_list(Start);
            false -> integer_to_list(Start) ++ "-" ++ integer_to_list(End)
        end,
    CommaStr = 
        case Acc =:= [] of
            true -> "";
            false -> ","
        end,
    index_str(Tail, RangeStr ++ CommaStr ++ Acc).
    

