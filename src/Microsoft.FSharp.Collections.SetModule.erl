-module('Microsoft.FSharp.Collections.SetModule').

-export([
         set/1,
         add/2,
         % contains/2,
         % count/1,
         difference/2,
         empty/0,
         % exist/2,
         % filter/2,
         % fold/3,
         % foldBack/3,
         % forall/2,
         % intersect/2,
         % intersectMany/2,
         % isEmpty/1,
         % isProperSubset/2,
         % isSubSet/2,
         % iter/2,
         % map/2,
         % maxElement/1,
         % minElement/1,
         ofArray/1,
         ofList/1,
         ofSeq/1,
         % partition/3,
         % remove/2,
         % singleton/1,
         % toArray/1,
         toList/1,
         % toSeq/1,
         union/2
         % unionMany/1
        ]).

% the set type is represented as a map
-type set() :: {set, #{term() => ok}}.

-export_type([set/0]).

empty() ->
    {set, #{}}.

add(Item, {set, Set}) ->
    {set, Set#{Item => ok}}.

difference({set, Set1}, {set, Set2}) ->
    {set, maps:without(maps:keys(Set2), Set1)}.

ofSeq({seq, _} = Seq) ->
    {set, 'Microsoft.FSharp.Collections.SeqModule':fold(fun (X, Acc) ->
                                                                Acc#{X => ok}
                                                        end, Seq, #{})}.

ofList(List) when is_list(List) ->
    {set, maps:from_list(lists:map(fun (I) -> {I, ok} end, List))}.

ofArray(Array) ->
    {set, array:foldl(fun (_, X, Acc) -> Acc#{X => ok} end, #{}, Array)}.

toList({set, Set}) ->
    maps:keys(Set).

union({set, Set1}, {set, Set2}) ->
    {set, maps:merge(Set1, Set2)}.

set(L) when is_list(L) ->
    ofList(L);
set({seq, _} = Seq) ->
    ofSeq(Seq);
set(Arr) ->
    case array:is_array(Arr) of
        true ->
            ofArray(Arr);
        false ->
            throw(argument_exception)
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

difference_test() ->
    S1 = set([1,2,3]),
    S2 = set([3,4]),
    [1,2] = toList(difference(S1, S2)),
    ok.

union_test() ->
    S1 = set([1,2,3]),
    S2 = set([3,4]),
    [1,2, 3,4] = toList(union(S1, S2)),
    ok.

set_test() ->
    _ = set([1]),
    _ = set({seq, {list, [1]}}),
    _ = set(array:from_list([1])),
    ok.

-endif.
