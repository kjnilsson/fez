-module(tester).
-export([square/1, squarelPlusOne/1, num/1, name/1, sum/1, just_n/1,
         blah/1, tuple/1, fact_fun/0, guards/1, maybe_head/1,
         listtest/1,
         strange_list/1
        ]).

-include_lib("test.hrl").

-record(a_rec, {num :: integer()}).

listtest(X) ->
    [A, _ | C] = X,
    {A, C}.

strange_list(X) when erlang:hd(X) =:= 5 -> {some, X};
strange_list(X) when tl(X) =:= [] -> none.

maybe_head([H | _Tail]) -> H;
maybe_head([]) -> undefined.

just_n(N) ->
    N2 = N,
    N2.

-spec square(integer()) -> integer().
square(X) -> X * X.

squarelPlusOne(X) ->
    X2 = X * X,
    X2 + 1.

num(#a_rec{num = Num}) ->
    Num.

name(#shrd_rec{name = Name}) -> Name.

sum(0) -> 0;
sum(1 = N) -> 1 + N;
sum(N) -> sum(N-1) + N.

blah(X) when X < 1 -> "no";
blah(_X) -> "yes".

tuple({1, S}) -> S;
tuple({2, "two" = S}) -> S;
tuple({N, _}) when N > 2 andalso N < 10 -> "between";
tuple(T) when element(1,T) =:= 11 -> "eleven";
tuple({_, _}) -> "many".

fact_fun() ->
    fun Fact(0) -> 1;
        Fact(N) -> N * Fact(N - 1)
    end.

guards(X) when is_integer(X) andalso X > 10 andalso X < 20 -> X-1;
guards(X) when is_integer(X) andalso (X > 10 orelse X == 5) -> X+1;
guards(X) -> X.
