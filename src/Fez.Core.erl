-module('Fez.Core').
-compile(export_all).
-compile({no_auto_import,[self/0, spawn/1]}).

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
    % try to get the type info out of the type instance
    {Mod, Type} = element(1, Instance),
    F = list_to_atom(atom_to_list(Type) ++ "." ++ atom_to_list(Function)),
    erlang:apply(Mod, F, Args).
