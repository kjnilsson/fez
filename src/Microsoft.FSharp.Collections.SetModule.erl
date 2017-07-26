-module('Microsoft.FSharp.Collections.SetModule').

-export([
         add/2,
         % contains/2,
         % count/1,
         % difference/2,
         empty/0
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
         % ofArray/1,
         % ofList/1,
         % ofSeq/1,
         % partition/3,
         % remove/2,
         % singleton/1,
         % toArray/1,
         % toList/1,
         % toSeq/1,
         % union/2,
         % unionMany/1
        ]).

% the set type is represented as a map
-type set() :: {set, #{term() => ok}}.

-export_type([set/0]).

empty() ->
    {set, #{}}.

add(Item, {set, Set}) ->
    {set, Set#{Item => ok}}.
