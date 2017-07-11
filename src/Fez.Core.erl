-module('Fez.Core').
-compile({no_auto_import,[self/0, spawn/1]}).

-export([
         self/0,
         spawn/1,
         'op_LessBang'/0,
         trait_call/3,
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
                Dst ! Msg
            end
    end.

trait_call(Instance, Function, Args) ->
    % assume the first tuple element is the type and thus
    % the module
    Mod = element(1, Instance),
    erlang:apply(Mod, Function, Args).

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
