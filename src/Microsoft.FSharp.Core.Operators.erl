-module('Microsoft.FSharp.Core.Operators').
-compile(export_all).

op_ComposeRight(_f0,_g0) ->
    fun (_x0) -> _g0 (_f0 (_x0)) end.

op_ComposeLeft(_g0,_f0) ->
        fun (_x0) -> _g0 (_f0 (_x0)) end.

op_PipeLeft(F, A) -> F(A).
op_PipeRight(A, F) -> F(A).

op_Range(Start, Finish)
  when is_integer(Start)
       andalso is_integer(Finish) ->
    lists:seq(Start, Finish).

op_RangeStep(Start, Step, Finish)
  when is_integer(Start)
       andalso is_integer(Step)
       andalso is_integer(Finish) ->
    lists:seq(Start, Finish, Step).

seq({seq, _} = X) -> X;
seq(X) when is_list(X) ->
    {seq, fun () -> {list, X} end}.

string(X) ->
    % primitive ToString
    io:format("~p", [X]).

fst(T) -> element(1, T).
snd(T) -> element(2, T).

box(X) -> X.
unbox(X) -> X.

failwith(M) ->
    throw({exception, M}).

reraise() ->
    % pick exception out of process dictionary
    case get(last_exception) of
        undefined -> throw(reraise);
        E -> throw(E)
    end.
