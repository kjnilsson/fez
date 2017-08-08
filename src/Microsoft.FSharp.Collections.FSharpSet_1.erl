-module('Microsoft.FSharp.Collections.FSharpSet_1').

-export([
         op_Subtraction/2,
         op_Addition/2
        ]).


op_Subtraction(S1, S2) ->
    'Microsoft.FSharp.Collections.SetModule':difference(S1, S2).

op_Addition(S1, S2) ->
    'Microsoft.FSharp.Collections.SetModule':union(S1, S2).
