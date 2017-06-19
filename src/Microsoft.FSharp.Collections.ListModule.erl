-module('Microsoft.FSharp.Collections.ListModule').
-export(
    [
        append/2,
        average/1,
        averageBy/2,
        averageBy/4,
        choose/2,
        chunkBySize/2,
        concat/1,
        collect/2,
        compareWith/3,
        contains/2,
        countBy/2,
        distinct/1,
        empty/0,
        exists/2,
        exists2/3,
        filter/2,
        find/2,
        findIndex/2,
        fold/3,
        fold2/4,
        foldBack/3,
        foldBack2/4,
        forall/2,
        forall2/3,
        head/1,
        init/2,
        init/4,
        isEmpty/1,
        iter/2,
        iter2/3,
        iteri/2,
        iteri/3,
        iteri2/3,
        iteri2/4,
        length/1,
        map/2,
        map/3,
        map2/3,
        map2/4,
        map3/4,
        map3/5,
        mapi/2,
        mapi/4,
        mapi2/3,
        mapi2/5,
        max/1,
        maxBy/2,
        maxBy/3,
        min/1,
        minBy/2,
        minBy/3,
        nth/2,
        partition/2,
        sort/1,
        last/1,
        rev/1,
        sum/1
    ]
).

append(L1, L2) ->
    lists:append(L1, L2).

%% Provides the average element of a list.
%% If the list is empty the case is not handle an will normally throw an exception
average(L) -> averageBy(fun(X) -> X end,L).

%% Provides the average element of a list but once each
%% element of the list was applied a projection function.
%% If the list is empty the case is not handle an will normally throw an exception
averageBy(Projection,L) ->
    averageBy(Projection,L,0,0).
averageBy(Projection,[X],AccSum,AccLength) ->
    AccSum + Projection(X) / AccLength;
averageBy(Projection,[First|Rest],AccSum,AccLength) ->
    averageBy(Projection, Rest, AccSum + First, AccLength + 1).


choose(Fun, L) ->
    lists:filter(fun (E) -> Fun(E) =/= undefined end, L).


chunkBySize(Len,List) ->
    L =
        lists:foldl(
            fun (E, []) -> [[E]];
                (E, [H|RAcc]) when erlang:length(H) < Len  -> [H ++ [E]|RAcc] ;
                (E, [H|RAcc]) -> [[E],H|RAcc]
            end
            , []
            , List),
    lists:reverse(L).

collect(Fun, L) ->
    lists:flatten(lists:map(Fun, L)).

compareWith(Fun, [H1|T1], [H2|T2]) ->
    case Fun(H1,H2) of
        0 -> compareWith(Fun, T1, T2) ;
        X -> X 
    end;
compareWith(_, [], []) -> 0;
compareWith(_, [], _) -> -1;
compareWith(_, _, []) -> 1.    

concat(SeqOfList) ->
    concat(SeqOfList,[]).
concat([],Acc) ->
    Acc;
concat([Head|Tail],Acc) -> 
    concat(Tail,lists:append(Head,Acc)).

contains(I, L) ->
    lists:member(I, L).

countBy(Generator,List) ->
    L1 = map(Generator,List),
    L2 = lists:sort(L1),
    countBy(L2,[],0).
countBy([],Acc,_) ->
    Acc;
countBy([H1],Acc,Counter) ->
    [{H1,Counter + 1}|Acc];    
countBy([H1|Tail],Acc,Counter) when H1 =:= hd(Tail) ->
    countBy(Tail,Acc,Counter+1);
countBy([H1|Tail],Acc,Counter) when H1 /= hd(Tail) ->
    countBy(Tail,[{H1,Counter+1}|Acc],0).



distinct(List) ->
    L = lists:sort(List),
    distinct(L,[]).
distinct([],Acc) ->
    Acc;
distinct([H1],Acc) ->
    [H1|Acc];    
distinct([H1|Tail],Acc) when H1 =:= hd(Tail) ->
    distinct(Tail,Acc);
distinct([H1|Tail],Acc) when H1 /= hd(Tail) ->
    distinct(Tail,[H1|Acc]).


empty() -> [].

exists(Pred, L) ->
    lists:any(Pred, L).

exists2(_,[],[]) -> false;
exists2(_,[],_) -> 
    erlang:error(badarg);
exists2(_,_,[]) -> 
    erlang:error(badarg);
exists2(Pred,[H1|T1],[H2|T2]) ->
    case Pred(H1,H2) of
        true -> true;
        false -> exists2(Pred,T1,T2)
    end.

filter(Pred, L) ->
    lists:filter(Pred, L).

find(Pred, L) ->
    head(lists:dropwhile(fun (I) -> not Pred(I) end, L)).

findIndex(Pred,L) ->
    findIndex(Pred,L,0) .
findIndex(_,[],_) ->
    erlang:error(badarg);
findIndex(Pred,[H|T],Index) ->
    case Pred(H) of
        true -> Index;
        false -> findIndex(Pred,T,Index+1)
    end.

fold(_, State, []) -> 
    State;
fold(Fold, State, [H|T]) -> 
    fold(Fold, Fold(State,H), T).

fold2(_,State,[],[]) -> 
    State;
fold2(_,_,[],_) -> 
    erlang:error(badarg);
fold2(_,_,_,[]) -> 
    erlang:error(badarg);
fold2(Fold,State,[H1|T1],[H2|T2]) -> 
    fold2(Fold,Fold(State,H1,H2),T1,T2).

foldBack(Fold,L,State) ->
    lists:foldr(Fold,State,L).

foldBack2(_,[],[],State) -> 
    State;
foldBack2(_,[],_,_) -> 
    erlang:error(badarg);
foldBack2(_,_,[],_) -> 
    erlang:error(badarg);
foldBack2(Fold,[H1|T1],[H2|T2],State) -> 
    Fold(H1,H2,foldBack2(Fold,T1,T2,State)).    


forall(Pred,L) -> 
    lists:all(Pred,L).

forall2(_,[],[]) -> 
    true;
forall2(_,[],_) -> 
    erlang:error(badarg);
forall2(_,_,[]) -> 
    erlang:error(badarg);
forall2(Pred,[H1|T1],[H2|T2]) ->
    case Pred(H1,H2) of
        true -> forall2(Pred,T1,T2);
        false -> false
    end.

head([H | _]) -> H.

init(Len,Init) ->
    init(Len,Init,[],0).
init(Len,_,Acc,_) when Len == 0 ->
    lists:reverse(Acc);
init(Len,Init,Acc,Index) ->
    init(Len-1,Init,[Init(Index)|Acc],Index+1).

isEmpty([]) -> true;
isEmpty([_|_]) -> false.

iter(Iter,L) -> lists:foreach(Iter,L).

iter2(_,[],[]) -> 
    ok;
iter2(_,[],_) -> 
    erlang:error(badarg);
iter2(_,_,[]) -> 
    erlang:error(badarg);
iter2(Iter,[H1|T1],[H2|T2]) ->
    Iter(H1,H2),
    iter2(Iter,T1,T2).


iteri(Iteri,L) ->
    iteri(Iteri,L,0).
iteri(_,[],_) ->
    ok;
iteri(Iteri,[H|T],Index) ->
    Iteri(Index,H),
    iteri(Iteri,T,Index+1).

iteri2(Iteri,L1,L2) ->
    iteri2(Iteri,L1,L2,0).
iteri2(_,[],[],_) -> 
    ok;
iteri2(_,[],_,_) -> 
    erlang:error(badarg);
iteri2(_,_,[],_) -> 
    erlang:error(badarg);
iteri2(Iter,[H1|T1],[H2|T2],Index) ->
    Iter(Index,H1,H2),
    iteri2(Iter,T1,T2,Index+1).

length(L) -> erlang:length(L).


map(Fun,List) ->
    map(Fun,List,[]).
map(_,[],Acc) -> 
    lists:reverse(Acc);
map(Fun,[H|T],Acc) -> 
    map(Fun,T,[Fun(H)|Acc]).

map2(Fun,L1,L2) ->
    map2(Fun,L1,L2,[]).
map2(_,[],[],Acc) -> 
    lists:reverse(Acc);
map2(_,[],_,_) -> 
    erlang:error(badarg);
map2(_,_,[],_) -> 
    erlang:error(badarg);
map2(Fun,[H1|T1],[H2|T2],Acc) -> 
    map2(Fun,T1,T2,[Fun(H1,H2)|Acc]).

map3(Fun,L1,L2,L3) ->
    map3(Fun,L1,L2,L3,[]).
map3(_,[],[],[],Acc) -> 
    lists:reverse(Acc);
map3(_,[],_,_,_) -> 
    erlang:error(badarg);
map3(_,_,[],_,_) -> 
    erlang:error(badarg);
map3(_,_,_,[],_) -> 
    erlang:error(badarg);
map3(Fun,[H1|T1],[H2|T2],[H3|T3],Acc) -> 
    map3(Fun,T1,T2,T3,[Fun(H1,H2,H3)|Acc]).



mapi(Mapi,List) ->
    mapi(Mapi,List,[],0).
mapi(_,[],Acc,_) -> 
    lists:reverse(Acc);
mapi(Mapi,[H|T],Acc,Index) -> 
    mapi(Mapi,T,[Mapi(Index,H)|Acc],Index+1).

mapi2(Mapi,L1,L2) ->
    mapi2(Mapi,L1,L2,[],0).
mapi2(_,[],[],Acc,_) -> 
    lists:reverse(Acc);
mapi2(_,_,[],_,_) -> 
    erlang:error(badarg);
mapi2(_,[],_,_,_) -> 
    erlang:error(badarg);
mapi2(Mapi,[H1|T1],[H2|T2],Acc,Index) -> 
    mapi2(Mapi,T1,T2,[Mapi(Index,H1,H2)|Acc],Index+1).

max(List) -> lists:max(List).


maxBy(_,[]) ->
    erlang:error(badarg);
maxBy(Projection,[H|T]) ->
    maxBy(Projection,T,H).
maxBy(_,[],_) ->
    erlang:error(badarg);
maxBy(Projection,[H],Max) ->
    erlang:max(Projection(H),Max);
maxBy(Projection,[H|T],Max) ->
    maxBy(Projection,T,erlang:max(Max,Projection(H))).

min(List) -> lists:min(List).

minBy(_,[]) ->
    erlang:error(badarg);
minBy(Projection,[H|T]) ->
    minBy(Projection,T,H).
minBy(_,[],_) ->
    erlang:error(badarg);
minBy(Projection,[H],Max) ->
    erlang:max(Projection(H),Max);
minBy(Projection,[H|T],Max) ->
    minBy(Projection,T,erlang:min(Max,Projection(H))).

nth(List,N) -> lists:nth(N,List).

% ofArray

% ofSeq

partition(Partition,List) -> lists:partition(Partition,List).

% permute

% pick()

sort(L) ->
    lists:sort(L).

last(L) ->
    lists:last(L).

rev(L) ->
    lists:reverse(L).

sum(L) ->
    lists:sum(L).


