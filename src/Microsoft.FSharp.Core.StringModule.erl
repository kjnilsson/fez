-module('Microsoft.FSharp.Core.StringModule').
-compile(export_all).

length(S) when is_list(S) ->
    erlang:length(S).

string(X) -> X.
