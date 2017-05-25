-module('Microsoft.FSharp.Core.OptionsModule').
-compile(export_all).

%% options are erased
-type option() :: term() | undefined.

-spec get(option()) -> term().
get(O) when O =/= undefined ->
    O.
