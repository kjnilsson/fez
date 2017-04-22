-module('ListModule').
-compile(export_all).

map(F, L) -> lists:map(F, L).
sort(L) -> lists:sort(L).
fold(F, S, L) -> lists:foldl(F, S, L).


