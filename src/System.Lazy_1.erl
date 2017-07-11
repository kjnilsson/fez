-module('System.Lazy_1').

-export([
         'get_IsValueCreated'/1,
         'get_Value'/1
        ]).

% -type lazy() :: {lazy, reference(), fun(() -> term())}.

'get_IsValueCreated'({lazy, Ref, _}) ->
    get(Ref) =/= undefined.

'get_Value'({lazy, Ref, Fun}) ->
    case get(Ref) of
        undefined ->
            V = Fun(),
            put(Ref, V),
            V;
        V -> V
    end.
