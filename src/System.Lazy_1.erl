-module('System.Lazy_1').

-export([
         'Create'/1,
         'get_IsValueCreated'/1,
         'Force'/1,
         'get_Value'/1,
         'release'/1
        ]).

-type lazy() :: {lazy, reference(), fun(() -> term())}.

-spec 'Create'(fun(() -> term())) -> lazy().
'Create'(Fun) ->
    {lazy, make_ref(), Fun}.

'get_IsValueCreated'({lazy, Ref, _}) ->
    get(Ref) =/= undefined.

'Force'({lazy, Ref, Fun}) ->
    case get(Ref) of
        undefined ->
            V = Fun(),
            put(Ref, V),
            V;
        V -> V
    end.

'get_Value'(Lazy) ->
    'Force'(Lazy).

% return "option" as item may not exist
-spec release(lazy()) -> term() | undefined.
'release'({lazy, Ref, _}) ->
    erase(Ref).
