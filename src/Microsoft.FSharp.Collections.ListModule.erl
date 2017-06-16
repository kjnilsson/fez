-module('Microsoft.FSharp.Collections.ListModule').
-compile(export_all).

append(L1, L2) ->
    lists:append(L1, L2).

choose(Fun, L) ->
    lists:filter(fun (E) -> Fun(E) =/= undefined end, L).

collect(Fun, L) ->
    lists:flatten(lists:map(Fun, L)).

concat(L1, L2) ->
    lists:sort(lists:append(L1, L2)).

contains(I, L) ->
    lists:member(I, L).

distinct([]) -> [];
distinct([First|Rest]) ->
    [First | [El || El <- distinct(Rest), El /= First]].

empty() -> [].

exists(Pred, L) ->
    lists:any(Pred, L).

filter(Pred, L) ->
    lists:filter(Pred, L).

find(Pred, L) ->
    head(lists:dropwhile(fun (I) -> not Pred(I) end, L)).

head([H | _]) -> H.

map(F, L) ->
    lists:map(F, L).

sort(L) ->
    lists:sort(L).

last(L) ->
    lists:last(L).

fold(F, S, L) ->
    lists:foldl(fun (E, Acc) -> F(Acc, E) end, S, L).

rev(L) ->
    lists:reverse(L).

sum(L) ->
    lists:sum(L).


