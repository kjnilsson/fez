-module('Microsoft.FSharp.Control.LazyExtensions').

-export([
         'Lazy_1.Create'/1,
         'Lazy_1.Force'/1
        ]).

'Lazy_1.Create'(Fun) ->
    {lazy, make_ref(), Fun}.

'Lazy_1.Force'(Lazy) ->
    'System.Lazy_1':'get_Value'(Lazy).
