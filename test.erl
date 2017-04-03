-module(test).
-export([square/1, num/1, name/1]).

-include_lib("test.hrl").

-record(a_rec, {num :: integer()}).

-spec square(integer()) -> integer().
square(X) -> X * X.

num(#a_rec{num = Num}) ->
    Num.

name(#shrd_rec{name = Name}) -> Name.
