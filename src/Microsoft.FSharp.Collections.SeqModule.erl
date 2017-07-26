-module('Microsoft.FSharp.Collections.SeqModule').
-export([
         append/2,
         average/1,
         averageBy/2,
         %cache/1,
         %cast/1,
         choose/2,
         collect/2,
         %compareWith/3
         concat/1,
         contains/2,
         % countBy/2
         delay/1,
         %distinct/1,
         %distinctBy/2,
         empty/0,
         %exactlyOne/1,
         exists/2,
         %exists2/3,
         filter/2,
         find/2,
         findIndex/2,
         fold/3,
         %forall/2
         %forall2/3
         %groupBy/2,
         head/1,
         %init/2,
         %initInfinite/1,
         isEmpty/1,
         iter/2,
         %iter2/3,
         %iteri2,
         %last/1,
         %length/1,
         map/2,
         %map2/3,
         %mapi/2,
         % max/1,
         % maxBy/2,
         % min/1,
         % minBy/2,
         % nth/2,
         % ofArray/1,
         ofList/1,
         %pairwise/1,
         %pick/2,
         %readonly/1,
         %reduce/2,
         %scan/3,
         singleton/1,
         %skip/2,
         %skipWhile/3,
         %sort/1,
         %sortBy/2,
         %sum/1,
         %sumBy/2,
         %tail/1,
         take/2,
         %takeWhle/3,
         %toArray/1,
         toList/1,
         %truncate/2,
         %tryFind/2,
         %tryPick/2,
         %unfold/3,
         %where/2,
         %windowed/2,
         %zip/2,
         %zip3/3,
         seq/1
        ]).

-type enumerator() :: {list, non_neg_integer(), list()}.

-type seq() :: {seq, enumerator()}.

-export_type([seq/0]).

append(Seq1, Seq2) ->
    {seq, {append, first, seq(Seq1), seq(Seq2)}}.

average(Seq) ->
    averageBy(fun id/1, Seq).

averageBy(F, Seq) ->
    Aggr = fun ({C, Sum}, N) -> {C+1, Sum + N} end,
    {Num, Total} =
        case next(seq(Seq)) of
            finished ->
                % no items in sequence
                throw(argument_exception);
            {Item, Enum} ->
                aggregate(Enum, {1, F(Item)}, Aggr)
        end,
    Total / Num.

empty() ->
    {seq, {list, []}}.

singleton(Item) ->
    {seq, {list, [Item]}}.

map(F, Seq) ->
    {seq, {map, F, seq(Seq)}}.

exists(F, Seq) ->
    find_internal(seq(Seq), F) =/= undefined.

filter(Pred, Seq) ->
    {seq, {filter, Pred, seq(Seq)}}.

find(Pred, Seq) ->
    case find_internal(seq(Seq), Pred) of
        undefined ->
            throw(key_not_found_exception);
        {_Index, Item} ->
            Item
    end.

findIndex(Pred, Seq) ->
    case find_internal(seq(Seq), Pred) of
        undefined ->
            throw(key_not_found_exception);
        {Index, _Item} ->
            Index
    end.

fold(Folder, State, Seq) ->
    aggregate(seq(Seq), State, Folder).

head(Seq) ->
    case next(seq(Seq)) of
        finished ->
            throw(argument_exception);
        {Item, _Seq} ->
            Item
    end.

init(Num, Gen) ->
    {seq, {iter, 0, Num, Gen}}.

isEmpty(Seq) ->
    next(seq(Seq)) =:= finished.

iter(Action, Seq) ->
    ignore = aggregate(Seq, ignore, fun (S, I) ->
                                            Action(I),
                                            S
                                    end),
    unit.


delay(F) ->
    {seq, {delay, F}}.

choose(Chooser, Seq) ->
    filter(fun(I) ->
                   Chooser(I) =/= undefined
           end, seq(Seq)).

collect(F, Sources) ->
    {seq, {collect, F, seq(Sources)}}.

concat(Sources) ->
    {seq, {concat, undefined, seq(Sources)}}.

contains(Item, Seq) ->
    find_internal(seq(Seq), fun (I) -> I =:= Item end) =/= undefined.

take(Num, Sources) ->
    {seq, {take, Num, seq(Sources)}}.

toList(Seq) ->
    enumerate(seq(Seq), []).

ofList(List) when is_list(List) ->
    seq(List).

% casts lists (and others) to seq
seq(L) when is_list(L) ->
    {seq, {list, L}};
seq({seq, _} = Seq) ->
    Seq;
seq(_Seq) ->
    throw(argument_exception).

%%% ------- internal -------

find_internal(Enum0, F) ->
    find_internal0(Enum0, 0, F).

find_internal0(Enum0, Index, F) ->
    case next(Enum0) of
        finished ->
            undefined;
        {Item, Enum} ->
            case F(Item) of
                true ->
                    {Index, Item};
                false ->
                    find_internal0(Enum, Index+1, F)
            end
    end.

aggregate(Enum0, State, F) ->
    case next(Enum0) of
        finished ->
            State;
        {Item, Enum} ->
            aggregate(Enum, F(State, Item), F)
    end.

enumerate(Enum0, Acc) ->
    case next(Enum0) of
        finished ->
            lists:reverse(Acc);
        {Item, Enum} ->
            enumerate(Enum, [Item | Acc])
    end.

next({seq, Enum}) ->
    next(Enum);
next({list, [H | Tail]}) ->
    {H, {list, Tail}};
next({list, []}) ->
    finished;
next({map, F, Enum0}) ->
    case next(Enum0) of
        finished -> finished;
        {Item, Enum} ->
            {F(Item), {map, F, Enum}}
    end;
next({filter, P, Enum}) ->
    do_filter(P, Enum);

next({take, 0, _Enum0}) ->
    finished;
next({take, Num, Enum0}) ->
    case next(Enum0) of
        finished ->
            % TODO: throw
            finished;
        {Item, Enum} ->
            {Item, {take, Num-1, Enum}}
    end;

next({append, first, Enum0, Seq2}) ->
    case next(Enum0) of
        finished ->
            next({append, second, Enum0, Seq2});
        {Item, Enum} ->
            {Item, {append, first, Enum, Seq2}}
    end;
next({append, second, Seq1Enum, Enum0}) ->
    case next(Enum0) of
        finished -> finished;
        {Item, Enum} ->
            {Item, {append, second, Seq1Enum, Enum}}
    end;

next({concat, undefined, Sources0}) ->
    case next(Sources0) of
        {Enum, Sources} ->
            next({concat, seq(Enum), Sources});
        finished ->
            finished
    end;
next({concat, Enum0, Sources0}) ->
    case next(Enum0) of
        finished ->
            case next(Sources0) of
                {Enum, Sources} ->
                    next({concat, seq(Enum), Sources});
                finished ->
                    finished
            end;
        {Item, Enum} ->
            {Item, {concat, Enum, Sources0}}
    end;

next({iter, N, N, _Gen}) ->
    finished;
next({iter, Count, Num, Gen}) ->
    {Gen(Count), {iter, Count+1, Num, Gen}};

next({delay, F}) ->
    next(seq(F()));
next({collect, F, {seq, Enum}}) ->
    % add empty "current" list
    next({collect, F, seq([]), Enum});
next({collect, F, Current0, Enum0}) ->
    case next(Current0) of
        finished ->
            case next(Enum0) of
                finished -> finished;
                {Item, Enum} ->
                    ItemsSeq = F(Item),
                    next({collect, F, seq(ItemsSeq), Enum})
            end;
        {Item, Current} ->
            {Item, {collect, F, Current, Enum0}}
    end.


do_filter(P, Enum0) ->
    case next(Enum0) of
        finished -> finished;
        {Item, Enum} ->
            case P(Item) of
                true ->
                    {Item, {filter, P, Enum}};
                false ->
                    do_filter(P, Enum)
            end
    end.

id(X) -> X.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

basics_test() ->
    EmptySeq = empty(),
    [] = toList(EmptySeq),
    Singleton = singleton(1),
    [1] = toList(Singleton),
    ListSeq = ofList([1,2,3]),
    [1,2,3] = toList(ListSeq),
    MapSeq = map(fun(N) -> N * 2 end, ListSeq),
    [2,4,6] = toList(MapSeq),
    MapSeq2 = map(fun(N) -> N * 2 end, MapSeq),
    [4,8,12] = toList(MapSeq2),
    Filter8 = filter(fun(N) -> N =:= 8 end, MapSeq2),
    [8] = toList(Filter8),
    Appended = append(ListSeq, MapSeq),
    [1,2,3,2,4,6] = toList(Appended),
    [1,2,3] = toList(delay(fun () -> ofList([1,2,3]) end)),
    ok.

average_test() ->
    2.0 = average([1.0,2.0,3.0]),
    2.0 = averageBy(fun float/1, [1,2,3]),
    ok.

fold_test() ->
    S = seq([1,2,3]),
    6 = fold(fun (State, T) -> State + T end, 0, S),
    99 = fold(fun (State, T) -> State + T end, 99, []).

choose_test() ->
    S = seq([1,2,3]),
    [2,3] = toList(choose(fun(1) -> undefined;
                             (N) -> N
                          end, S)).

concat_test() ->
    Sources = seq([seq([1,2,3]), [4,5,6]]),
    [1,2,3,4,5,6] = toList(concat(Sources)).

contains_test() ->
    Seq = seq([1,2,3]),
    true = contains(2, Seq),
    false = contains(5, Seq),
    ok.

find_test() ->
    Seq = seq([1,2,3]),
    2 = find(fun (I) -> I > 1 end, Seq),
    2 = findIndex(fun (I) -> I == 3 end, Seq),
    true = exists(fun (I) -> I == 3 end, Seq),
    false = exists(fun (I) -> I == 4 end, Seq),
    ok.

head_test() ->
    1 = head([1,2,3]),
    ok.

isEmpty_test() ->
    false = isEmpty([1,2,3]),
    true = isEmpty(seq([])),
    ok.

iter_test() ->
    Seq = seq([1,2,3]),
    iter(fun(I) -> put(iter_test, I) end, Seq),
    3 = get(iter_test).

init_test() ->
    [0,1,2] = toList(init(3, fun(I) -> I end)),
    ok.

lists_are_seqs_test() ->
    [1,2,3] = toList([1,2,3]),
    [1,2,3] = toList(delay(fun () -> [1,2,3] end)),
    [1,2] = toList(append([1], [2])),
    ok.

collect_test() ->
    [1, -1, 2, -2, 3, -3] = toList(collect(fun (X) -> [X, -X] end, [1,2,3])),
    ok.

take_test() ->
    [1,2] = toList(take(2, [1,2,3])),
    ok.

-endif.
