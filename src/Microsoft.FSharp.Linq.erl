-module('Microsoft.FSharp.Linq').

-export([
         'QueryBuilder.Run'/2,
         'QueryBuilder.Source'/2,
         'QueryBuilder.Yield'/2,
         'QueryBuilder.For'/3,
         'QueryBuilder.Where'/3,
         'QueryBuilder.Select'/3
        ]).


'QueryBuilder.Run'(_Builder, {qs, Seq}) ->
    Seq.

'QueryBuilder.Source'(_Builder, Seq) ->
    {qs, Seq}.

'QueryBuilder.Yield'(_Builder, Item) ->
    {qs, 'Microsoft.FSharp.Collections.SeqModule':singleton(Item)}.

'QueryBuilder.For'(_Builder, {qs, Seq}, ProjectionFun) ->
    F = fun(Item) -> element(2, ProjectionFun(Item)) end,
    {qs, 'Microsoft.FSharp.Collections.SeqModule':collect(F, Seq)}.

'QueryBuilder.Where'(_Builder, {qs, Seq}, Predicate) ->
    {qs, 'Microsoft.FSharp.Collections.SeqModule':filter(Predicate, Seq)}.

'QueryBuilder.Select'(_Builder, {qs, Seq}, Projection) ->
    {qs, 'Microsoft.FSharp.Collections.SeqModule':map(Projection, Seq)}.
