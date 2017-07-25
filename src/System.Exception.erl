-module('System.Exception').

-export([
         '.ctor'/1
        ]).

'.ctor'(Msg) ->
    % record structure
    {'System.Exception', Msg}.
