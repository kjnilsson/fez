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
        'FSharpAsync.Sleep'/1

        ]).

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
                           timer:sleep(T),
                           {async, return, unit}
                   end}.

% TODO: can we use token somehow to cancel the async? how would we register?
'FSharpAsync.Start'(Async, _Token) ->
    % run async on another process
    _ = spawn(fun () -> run(Async) end),
    ok.

'FSharpAsync.StartChild'(Async, Timeout0) ->
    Timeout = case Timeout0 of
                  undefined -> infinity;
                  _ -> Timeout0
              end,
    Ref = make_ref(),
    {async, delay,
     fun () ->
             Self = self(),
             % TODO: monitor or link to process
             Pid = spawn(fun () ->
                                 % run async
                                 Result = run(Async),
                                 receive
                                     {get_result, Ref, From} ->
                                         From ! {result, Ref, Result}
                                 after Timeout ->
                                           Self ! async_start_child_timeout
                                 end
                         end),
             Pid ! {get_result, Ref, Self},
             % return an async that receives the result
             {async, return,
              {async, delay, fun () ->
                                    receive
                                        {result, Ref, Result} ->
                                            {async, return, Result}
                                    after Timeout ->
                                              throw(async_start_child_timeout)
                                    end
                            end}}
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
