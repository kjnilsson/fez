-module('Microsoft.FSharp.Control.FSharpAsync').

-export([
        'Start'/2,
        'StartChild'/2,
        'RunSynchronously'/3,
        'Ignore'/1,
        'Sleep'/1,
        'Parallel'/1
        ]).

-define(SEQMOD, 'Microsoft.FSharp.Collections.SeqModule').

% -type async() :: {async, delay | return | bind | zero, term()}.

'Sleep'(T) ->
    {async, delay, fun() ->
                           ok = timer:sleep(T),
                           {async, return, unit}
                   end}.

'Ignore'(Async) ->
    {async, delay, fun() ->
                           _ = run(Async),
                           {async, return, unit}
                   end}.

% TODO: can we use token somehow to cancel the async? how would we register?
'Start'(Async, undefined) ->
    % run async on another process
    _ = spawn(fun () -> run(Async) end),
    ok;
'Start'(_Async, _Token) ->
    exit({fez_unsupported, "Start cannot be used with"
                           "a CancellationToken"}).

'StartChild'(Async, Timeout0) ->
    Timeout = timeout(Timeout0),
    {async, delay,
     fun () ->
             Ref = make_ref(),
             _Pid = spawn_child(Async, Ref, Timeout),
             % return an async that receives the result
             {async, return,
              {async, delay, fun () -> receive_result(Ref, Timeout) end}}
     end}.

'Parallel'(Asyncs0) ->
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

'RunSynchronously'(Async, undefined, undefined) ->
    run(Async);
'RunSynchronously'(_Async, _Timeout, _Token) ->
    exit({fez_unsupported, "RunSynchronously cannot be used with"
                           "a Timeout or CancellationToken"}).

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
            {async, return, Result};
        {error, Ref, Err} ->
            throw(Err)
    after Timeout ->
              throw(async_start_child_timeout)
    end.

spawn_child(Async, Ref, Timeout) ->
    Spawner = self(),
    Pid = spawn(fun () ->
                        % run async
                        Reply = try run(Async) of
                                    R -> {result, Ref, R}
                                catch
                                    _:_ = Err ->
                                        {error, Ref, Err}
                                end,

                        receive
                            {get_result, Ref, From} ->
                                From ! Reply
                        after Timeout ->
                                  Spawner ! {error, Ref, async_start_child_timeout}
                        end
                end),
     Pid ! {get_result, Ref, self()},
     Pid.


