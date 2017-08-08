-module('Microsoft.FSharp.Core.ExtraTopLevelOperators').
-export([
         async/0,
         printfn/1,
         query/0,
         set/1
        ]).

% dummy value for async builder
async() -> ok.

printfn(S) ->
    io:format(S, []).

query() -> ok.

set(S) ->
    'Microsoft.FSharp.Collections.SetModule':set(S).
