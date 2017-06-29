-module('Microsoft.FSharp.Core.ExtraTopLevelOperators').
-compile(export_all).

printfn(S) ->
    io:format(S, []).

query() -> query_builder.
