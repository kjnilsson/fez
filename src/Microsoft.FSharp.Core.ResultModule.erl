-module('Microsoft.FSharp.Core.ResultModule').

-export([
         map/2,
         mapError/2,
         bind/2
        ]).

% Result representation
-type result() :: {ok, term()} | {error, term()}.

-export_type([result/0]).

map(F, {ok, V}) ->
    {ok, F(V)};
map(_F, R) ->
    R.


mapError(F, {error, Err}) ->
    {error, F(Err)};
mapError(_F, R) ->
    R.

bind(Binder, {ok, V}) ->
    Binder(V);
bind(_Binder, R) ->
    R.
