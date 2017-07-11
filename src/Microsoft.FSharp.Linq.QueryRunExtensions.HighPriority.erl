-module('Microsoft.FSharp.Linq.QueryRunExtensions.HighPriority').

-export([
         'QueryBuilder.Run'/2
        ]).


'QueryBuilder.Run'(_Builder, {qs, Seq}) ->
    Seq.
