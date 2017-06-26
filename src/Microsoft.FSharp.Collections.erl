-module('Microsoft.FSharp.Collections').

-export([
         'FSharpSet_1.Add'/2
        ]).

% this module holds member calls to standard FSharp types (list, set, map etc)

'FSharpSet_1.Add'(Set, Item) ->
    'Microsoft.FSharp.Collections.SetModule':'add'(Item, Set).

