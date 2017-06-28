-module('Microsoft.FSharp.Core').
% members on fsharp created types typically go here

-export([
         'FSharpRef_1.get_Value'/1,
         'FSharpRef_1.release'/1
        ]).

'FSharpRef_1.get_Value'({ref, Key}) ->
    get(Key).

'FSharpRef_1.release'({ref, Key}) ->
    erase(Key).
