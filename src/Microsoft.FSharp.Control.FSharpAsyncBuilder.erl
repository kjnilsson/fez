-module('Microsoft.FSharp.Control.FSharpAsyncBuilder').

-export([
        'Delay'/2,
        'Return'/2,
        'ReturnFrom'/2,
        'Bind'/3,
        'Zero'/1
        ]).

-define(SEQMOD, 'Microsoft.FSharp.Collections.SeqModule').

% -type async() :: {async, delay | return | bind | zero, term()}.

'Delay'(_B, Fun) ->
    {async, delay, Fun}.

'Return'(_B, V) ->
    {async, return, V}.

'ReturnFrom'(_B, Async) ->
    %% is this right?
    {async, return_from, Async}.

'Bind'(_B, Async, Binder) ->
    {async, bind, {Async, Binder}}.

'Zero'(_B) ->
    {async, zero, unit}.
