-module('Microsoft.FSharp.Collections.ListModule').
-compile(export_all).

map(F, L) -> lists:map(F, L).
sort(L) -> lists:sort(L).
last(L) -> lists:last(L).
fold(F, S, L) -> lists:foldl(F, S, L).
rev(L) -> lists:reverse(L).


