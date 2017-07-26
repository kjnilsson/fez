-module('Microsoft.FSharp.Collections.SeqModule').
-export([
         append/2,
         %average/1,
         %averageBy/2,
         %cache/1,
         %cast/1,
         %choose/2,
         collect/2,
         %compareWith/3
         %concat/2
         %contains/2
         %countBy/2
         delay/1,
         %distinct/1,
         %distinctBy/2,
         empty/0,
         %exactlyOne/1,
         %exists/2,
         %exists2/3,
         filter/2,
         %find/2,
         %findIndex/2,
         %fold/3,
         %forall/2
         %forall2/3
         %groupBy/2,
         %head/1,
         %init/2,
         %initInfinite/1,
         %isEmpty/1,
         %iter/2,
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

empty() ->
    {seq, {list, []}}.

singleton(Item) ->
    {seq, {list, [Item]}}.

append(Seq1, Seq2) ->
    {seq, {append, first, seq(Seq1), seq(Seq2)}}.

map(F, Seq) ->
    {seq, {map, F, seq(Seq)}}.

filter(Pred, Seq) ->
    {seq, {filter, Pred, seq(Seq)}}.

delay(F) ->
    {seq, {delay, F}}.

collect(F, Sources) ->
    {seq, {collect, F, seq(Sources)}}.

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
    Seq.

%%% ------- internal -------

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
