-module('Fez.Core').
-compile({no_auto_import,[self/0, spawn/1]}).

-export([
         self/0,
         spawn/1,
         'op_LessBang'/0,
         trait_call/3,
         field_set/3,
         field_get/2,
         inherit/2,
         multi_dispatch/3,
         fast_integer_loop/3,
         'Lazy_1.release'/1,
         'Ref_1.release'/1
        ]).
self() -> erlang:self().
spawn(F) -> erlang:spawn(F).

% the ubiquitous message sending function
'op_LessBang'() ->
    fun (Dst) ->
            fun (Msg) ->
                Dst ! Msg,
                unit
            end
    end.

trait_call(Instance, Function, Args) ->
    % assume the first tuple element is the type and thus
    % the module
    Mod = element(1, Instance),
    erlang:apply(Mod, Function, Args).

multi_dispatch({Type, Bases, _Closure}, Function, Args) ->
    dispatch([Type | Bases], Function, Args).

dispatch([], F, _Args) ->
    exit({function_not_exported, F});
dispatch([T | Rest], F, Args) ->
    case erlang:function_exported(T, F, length(Args)) of
        true ->
            erlang:apply(T, F, Args);
        false ->
            dispatch(Rest, F, Args)
    end.


inherit(Type, {BaseType, Bases, Closure}) ->
    {Type, [BaseType | Bases], Closure}.

field_set(Field, Value, {Type, Bases, Closure}) ->
    {Type, Bases, maps:put(Field, Value, Closure)}.

field_get(Field, {_Type, _Bases, Closure}) ->
    maps:get(Field, Closure).

fast_integer_loop(From, To, _Fun) when From > To ->
    unit;
fast_integer_loop(From, To, Fun) ->
    [Fun(I) || I <- lists:seq(From, To)],
    unit.



% return "option" as item may not exist
% -spec 'Lazy_1.release'(lazy()) -> term() | undefined.
'Lazy_1.release'({lazy, Ref, _}) ->
    erase(Ref).

'Ref_1.release'({ref, Key}) ->
    erase(Key).
