-module('Fez.Core').
-compile(export_all).
-compile({no_auto_import,[self/0, spawn/1]}).

self() -> erlang:self().
spawn(F) -> erlang:spawn(F).

'op_LessBang'() ->
    fun (Dst) ->
            fun (Msg) ->
                Dst ! Msg
            end
    end.
