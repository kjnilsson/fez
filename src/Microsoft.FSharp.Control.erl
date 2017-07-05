-module('Microsoft.FSharp.Control').

-export([
        'FSharpAsyncBuilder.Delay'/2,
        'FSharpAsyncBuilder.Return'/2,
        'FSharpAsyncBuilder.ReturnFrom'/2,
        'FSharpAsyncBuilder.Bind'/3,
        'FSharpAsyncBuilder.Zero'/1,
        'FSharpAsync.Start'/2,
        'FSharpAsync.StartChild'/2,
        'FSharpAsync.RunSynchronously'/3,
        'FSharpAsync.Ignore'/1,
        'FSharpAsync.Sleep'/1,
        'FSharpAsync.Parallel'/1
        ]).

-define(SEQMOD, 'Microsoft.FSharp.Collections.SeqModule').

% -type async() :: {async, delay | return | bind | zero, term()}.

'FSharpAsyncBuilder.Delay'(_B, Fun) ->
    {async, delay, Fun}.

'FSharpAsyncBuilder.Return'(_B, V) ->
    {async, return, V}.

'FSharpAsyncBuilder.ReturnFrom'(_B, Async) ->
    %% is this right?
    {async, return_from, Async}.

'FSharpAsyncBuilder.Bind'(_B, Async, Binder) ->
    {async, bind, {Async, Binder}}.

'FSharpAsyncBuilder.Zero'(_B) ->
    {async, zero, unit}.

'FSharpAsync.Sleep'(T) ->
    {async, delay, fun() ->
                           ok = timer:sleep(T),
                           {async, return, unit}
                   end}.

'FSharpAsync.Ignore'(Async) ->
    {async, delay, fun() ->
                           _ = run(Async),
                           {async, return, unit}
                   end}.
% TODO: can we use token somehow to cancel the async? how would we register?
'FSharpAsync.Start'(Async, _Token) ->
    % run async on another process
    _ = spawn(fun () -> run(Async) end),
    ok.

'FSharpAsync.StartChild'(Async, Timeout0) ->
    Timeout = timeout(Timeout0),
    {async, delay,
     fun () ->
             % TODO: monitor or link to process
             Ref = make_ref(),
             _Pid = spawn_child(Async, Ref, Timeout),
             % return an async that receives the result
             {async, return,
              {async, delay, fun () -> receive_result(Ref, Timeout) end}}
     end}.

'FSharpAsync.Parallel'(Asyncs0) ->
    Timeout = infinity, % why doesn't Parallel have a timeout?
    {async, delay,
     fun () ->
             Ref = make_ref(),
             Asyncs = ?SEQMOD:toList(Asyncs0),
             [_Pid = spawn_child(Async, Ref, Timeout) || Async <- Asyncs],
             Results = receive_n_results(Ref, Timeout,
                                         length(Asyncs),
                                         []),
             {async, return, array:from_list([run(A) || A <- Results])}
     end}.

'FSharpAsync.RunSynchronously'(Async, _Timeout, _Token) ->
    run(Async).

run({async, zero, unit}) -> ok;
run({async, delay, Fun}) ->
    run(Fun());
run({async, return, V}) ->
    V;
run({async, return_from, A}) ->
    run(A);
run({async, bind, {Async, Binder}}) ->
    case run(Async) of
        unit ->
            run(Binder());
        V ->
            run(Binder(V))
    end.

timeout(undefined) -> infinity;
timeout(T) -> T.

receive_n_results(_Ref, _Timeout, 0, Results) ->
    Results;
receive_n_results(Ref, Timeout, N, Results) ->
    Result = receive_result(Ref, Timeout),
    receive_n_results(Ref, Timeout, N-1, [Result | Results]).

receive_result(Ref, Timeout) ->
    receive
        {result, Ref, Result} ->
            {async, return, Result}
    after Timeout ->
              throw(async_start_child_timeout)
    end.

spawn_child(Async, Ref, Timeout) ->
    Spawner = self(),
    Pid = spawn(fun () ->
                  % run async
                  Result = run(Async),
                  receive
                      {get_result, Ref, From} ->
                          From ! {result, Ref, Result}
                  after Timeout ->
                            Spawner ! async_start_child_timeout
                  end
          end),
     Pid ! {get_result, Ref, self()},
     Pid.


