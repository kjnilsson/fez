-module('Microsoft.FSharp.Core.StringModule').
-compile(export_all).

%% TODO: at some point should strings be binaries just
%% like in elixir?

length(S) when is_list(S) ->
    erlang:length(S).

string(X) -> X.
