-module('System.Collections.Generic.List_1').

-export([
         '.ctor'/0,
         'Add'/2
        ]).

'.ctor'() ->
    Ref = make_ref(),
    put(Ref, {0, array:new()}),
    {resize_array, Ref}.

'Add'({resize_array, Ref}, Value) ->
    {Next, A} = get(Ref),
    put(Ref, {Next+1, array:set(Next, Value, A)}),
    unit.
