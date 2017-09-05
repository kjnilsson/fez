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
         forall/2,
         isEmpty/1,
         % iter/2,
         map/2,
         % ofArray/1,
         ofList/1,
         ofSeq/1,
         % partition/3,
         % pick/2,
         remove/2,
         % toArray/1,
         toList/1,
         toSeq/1,
         tryFind/2,
         tryFindKey/2,
         tryPick/2
        ]).

-define(SEQ_MOD, 'Microsoft.FSharp.Collections.SeqModule').

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

ofList(List) ->
    maps:from_list(List).

ofSeq(Seq) ->
    maps:from_list(?SEQ_MOD:toList(Seq)).

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

forall(Pred, M) ->
    maps:fold(fun (K, V, true) ->
                      Pred(K, V);
                  (_K, _V, false) ->
                      false
              end, true, M).

isEmpty(#{}) -> true;
isEmpty(_) -> false.

map(F, M) ->
    maps:map(F, M).

toList(M) ->
    maps:to_list(M).

toSeq(M) ->
    ?SEQ_MOD:seq(M).

tryFind(K, M) ->
    maps:get(K, M, undefined).

tryFindKey(K, M) ->
    maps:is_key(K, M).

tryPick(Chooser, M) ->
    % TODO: optimise
    maps:fold(fun (K, V, undefined) ->
                      case Chooser(K, V) of
                          undefined -> undefined;
                          V -> V
                      end;
                  (_K, _V, Acc) -> Acc
              end, undefined, M).


remove(K, M) ->
    maps:remove(K, M).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

ofSeq_test() ->
    Seq = ?SEQ_MOD:seq([{1,1}]),
    #{1 := 1} = ofSeq(Seq),
    ok.

toSeq_test() ->
    [{1,1}] = ?SEQ_MOD:toList(toSeq(#{1 => 1})),
    ok.

forall_test() ->
    Pred = fun (_, V) -> V < 5 end,
    true = forall(Pred, #{1 => 4}),
    false = forall(Pred, #{1 => 4, 2 => 5}),
    ok.


-endif.
