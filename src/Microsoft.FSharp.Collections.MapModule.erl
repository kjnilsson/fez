-module('Microsoft.FSharp.Collections.MapModule').
-compile(export_all).


add(K, V, M) ->
    maps:put(K, V, M).

containsKey(K, M) ->
    maps:is_key(K, M).

empty() -> #{}.

exists(P, M) ->
    maps:fold(fun (_, _, true) -> true;
                  (K, V, false) ->
                      P(K, V)
              end, false, M).

filter(P, M) ->
    maps:filter(P, M).

find(K, M) ->
    maps:get(K, M).

findKey(P, M) ->
    maps:fold(fun (K, V, undefined)  ->
                      case P(K, V) of
                          true -> K;
                          false -> undefined
                      end;
                          (_, _, K) -> K
                      end, undefined, M).

fold(F, S, M) ->
    maps:fold(fun (K, V, Acc) ->
                      F(Acc, K, V)
              end, S, M).

remove(K, M) ->
    maps:remove(K, M).

isEmpty(#{}) -> true;
isEmpty(_) -> false.

map(F, M) ->
    maps:map(F, M).

toList(M) ->
    maps:to_list(M).

tryFind(K, M) ->
    maps:get(K, M, undefined).

