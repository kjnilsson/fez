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
bind(Elevate,O) when O =/= undefined -> Elevate(O);
bind(_,O) when O =:= undefined -> undefined.

%% count : 'T option -> int
count(O) when O =/= undefined -> 1;
count(O) when O =:= undefined -> 0.

%% exists : ('T -> bool) -> 'T option -> bool
exists(_,O) when O =:= undefined -> false;
exists(Pred,O) when O =/= undefined -> Pred(O).

%% filter : ('T -> bool) -> option:'T option -> 'T option
%% TODO : Verify the correctness
filter(_,O) when O =:= undefined -> undefined;
filter(Pred,O) when O =/= undefined -> Pred(O).


%% fold : ('State -> 'T -> 'State) -> 'State -> 'T option -> 'State
fold(_,State,O) when O =:= undefined -> State;
fold(Fold,State,O) when O =/= undefined -> Fold(State,O).

%% foldBack : ('T -> 'State -> 'State) -> 'T option -> 'State -> 'State
foldBack(_,O,State) when O =:= undefined -> State;
foldBack(Fold,O,State) when O =/= undefined -> Fold(O,State).

%% forall : ('T -> bool) -> 'T option -> bool
forall(_,O) when O =:= undefined -> true;
forall(Pred,O) when O =/= undefined -> Pred(O).

-spec get(option()) -> term().
get(O) when O =/= undefined ->
    O.

isNone(O) -> O =:= undefined.

isSome(O) -> O =/= undefined.

%% iter : ('T -> unit) -> 'T option -> unit
iter(_,O) when O =:= undefined -> unit;
iter(Action,O) when O =/= undefined -> Action(O).

%% map : ('T -> 'U) -> 'T option -> 'U option
map(_,O) when O =:= undefined -> undefined;
map(Mapping,O) when O =/= undefined -> Mapping(O).

%% TODO : Implement once RFC has been settled
%% toArray(O) ->

toList(O) when O =:= undefined -> [];
toList(O) when O =/= undefined -> [O].


%% TODO : Implement once Nullable type is defined
%% toNullable(O) ->




