-module('Fez.Core.BinaryModule').

-export([
         at/2,
         part/3
         ]).


at(Pos, Subj) ->
    binary:at(Subj, Pos).

part(Pos, Len, Subj) ->
    binary:part(Subj, Pos, Len).




-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
