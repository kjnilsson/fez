-module('Microsoft.FSharp.Core.Operators').
-compile(export_all).

op_ComposeRight(_f0,_g0) ->
    fun (_x0) -> _g0 (_f0 (_x0)) end.

op_ComposeLeft(_g0,_f0) ->
        fun (_x0) -> _g0 (_f0 (_x0)) end.

op_PipeLeft(F, A) -> F(A).
op_PipeRight(A, F) -> F(A).

seq({seq, _} = X) -> X.
