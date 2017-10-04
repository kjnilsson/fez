-module('Microsoft.FSharp.Core.OptionsModule').
-export([
         bind/2,
         count/1,
         exists/2,
         filter/2,
         fold/3,
         foldBack/3,
         forall/2,
         'get'/1,
         isNone/1,
         isSome/1,
         iter/2,
         map/2,
         % toArray/1,
         toList/1
         % toNullable/1
        ]).

%% options are erased
-type option() :: term() | undefined.

%% ('T -> 'U option) -> 'T option -> 'U option
bind(_Binder, undefined) -> undefined;
bind(Binder, O) -> Binder(O).

%% count : 'T option -> int
count(undefined) -> 0;
count(_O) -> 1.

%% exists : ('T -> bool) -> 'T option -> bool
exists(_, undefined) -> false;
exists(Pred, O) -> Pred(O).

%% filter : ('T -> bool) -> option:'T option -> 'T option
%% TODO : Verify the correctness
filter(_Pred, undefined) -> undefined;
filter(Pred, O) ->
    case Pred(O) of
        true -> O;
        false -> undefined
    end.


%% fold : ('State -> 'T -> 'State) -> 'State -> 'T option -> 'State
fold(_Folder, State, undefined) -> State;
fold(Folder, State, O) -> Folder(State, O).

%% foldBack : ('T -> 'State -> 'State) -> 'T option -> 'State -> 'State
foldBack(_, undefined, State) -> State;
foldBack(Folder, O, State) -> Folder(O, State).

%% forall : ('T -> bool) -> 'T option -> bool
forall(_, undefined) -> true;
forall(Pred, O) -> Pred(O).

-spec get(option()) -> term().
get(O) when O =/= undefined ->
    O.

isNone(O) -> O =:= undefined.

isSome(O) -> O =/= undefined.

%% iter : ('T -> unit) -> 'T option -> unit
iter(_, undefined) -> unit;
iter(Action, O) -> Action(O).

%% map : ('T -> 'U) -> 'T option -> 'U option
map(_, undefined) -> undefined;
map(Mapping, O) -> Mapping(O).

%% TODO : Implement once RFC has been settled
% toArray(undefined) -> array:new(0, []);

toList(undefined) -> [];
toList(O) -> [O].


%% TODO : Implement once Nullable type is defined
%% toNullable(O) ->




