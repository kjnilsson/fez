-module('Microsoft.FSharp.Core.FSharpRef_1').
% members on fsharp created types typically go here

-export([
         'get_Value'/1
        ]).

'get_Value'({ref, Key}) ->
    get(Key).
