-module('Microsoft.FSharp.Control').

-export([
        'FSharpAsyncBuilder.Delay'/2,
        'FSharpAsyncBuilder.Return'/2,
        'FSharpAsyncBuilder.Bind'/3,
        'FSharpAsyncBuilder.Zero'/1,
        'FSharpAsync.Start'/2,
        'FSharpAsync.RunSynchronously'/3

        ]).

% -type async() :: {async, delay | return | bind | zero, term()}.

'FSharpAsyncBuilder.Delay'(_B, Fun) ->
    {async, delay, Fun}.

'FSharpAsyncBuilder.Return'(_B, V) ->
    {async, return, V}.

'FSharpAsyncBuilder.Bind'(_B, Async, Binder) ->
    {async, bind, {Async, Binder}}.

'FSharpAsyncBuilder.Zero'(_B) ->
    {async, zero, unit}.

% TODO: can we use token somehow to cancel the async? how would we register?
'FSharpAsync.Start'(Async, _Token) ->
    % run async on another process
    spawn(fun () -> run(Async) end),
    ok.

'FSharpAsync.RunSynchronously'(Async, _Timeout, _Token) ->
    run(Async).

run({async, zero, unit}) -> ok;
run({async, delay, Fun}) ->
    run(Fun());
run({async, return, V}) ->
    V;
run({async, bind, {Async, Binder}}) ->
    run(Binder(run(Async))).
