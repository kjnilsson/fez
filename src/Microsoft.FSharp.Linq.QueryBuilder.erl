-module('Microsoft.FSharp.Linq.QueryBuilder').

-export([
         'Run'/2,
         'Source'/2,
         'Yield'/2,
         'For'/3,
         'Where'/3,
         'Select'/3
        ]).


'Run'(_Builder, {qs, Seq}) ->
    Seq.

'Source'(_Builder, Seq) ->
    {qs, Seq}.

'Yield'(_Builder, Item) ->
    {qs, 'Microsoft.FSharp.Collections.SeqModule':singleton(Item)}.

'For'(_Builder, {qs, Seq}, ProjectionFun) ->
    F = fun(Item) -> element(2, ProjectionFun(Item)) end,
    {qs, 'Microsoft.FSharp.Collections.SeqModule':collect(F, Seq)}.

'Where'(_Builder, {qs, Seq}, Predicate) ->
    {qs, 'Microsoft.FSharp.Collections.SeqModule':filter(Predicate, Seq)}.

'Select'(_Builder, {qs, Seq}, Projection) ->
    {qs, 'Microsoft.FSharp.Collections.SeqModule':map(Projection, Seq)}.
