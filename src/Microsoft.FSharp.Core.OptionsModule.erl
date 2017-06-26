-module('Microsoft.FSharp.Core.OptionsModule').
-export([
         % bind/2,
         % count/1,
         % exists/2,
         % filter/1,
         % fold/3,
         % foldBack/3,
         % forall/2,
         'get'/1
         % isNone/1,
         % isSome/1,
         % iter/2,
         % map/2,
         % toArray/1,
         % toList/1,
         % toNullable/1
        ]).

%% options are erased
-type option() :: term() | undefined.

-spec get(option()) -> term().
get(O) when O =/= undefined ->
    O.
