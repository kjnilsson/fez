-module('Microsoft.FSharp.Collections.MapModule').
-export([
         add/3,
         containsKey/2,
         empty/0,
         exists/2,
         filter/2,
         find/2,
         findKey/2,
         fold/3,
         % foldBack/3,
         % forall/2,
         isEmpty/1,
         % iter/2,
         map/2,
         % ofArray/1,
         % ofList/1,
         % ofSeq/1,
         % partition/3,
         % pick/2,
         remove/2,
         % toArray/1,
         toList/1,
         % toSeq/1,
         tryFind/2
         % tryFindKey/3,
         % tryPick/2
        ]).


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

isEmpty(#{}) -> true;
isEmpty(_) -> false.

map(F, M) ->
    maps:map(F, M).

toList(M) ->
    maps:to_list(M).

tryFind(K, M) ->
    maps:get(K, M, undefined).

remove(K, M) ->
    maps:remove(K, M).

