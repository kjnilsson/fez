-module('Microsoft.FSharp.Core.Operators').
-compile(export_all).
-compile({no_auto_import,['not'/1]}).

op_ComposeRight(F,G) ->
    fun (X) -> G (F (X)) end.

op_ComposeLeft(G, F) ->
        fun (X) -> G (F (X)) end.

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

op_Modulus(X, Y) ->
    X rem Y.

op_Division(X, Y) ->
    X / Y.

op_Append(L1, L2) ->
    L1 ++ L2.

op_GreaterThanOrEqual(L1, L2) ->
    L1 >= L2.

op_LessThanOrEqual(L1, L2) ->
    L1 =< L2.

id(X) -> X.

ignore(_X) -> ok.

hash(T) ->
    erlang:phash2(T).

'not'(B) -> not B.

seq({seq, _} = X) -> X;
seq(X) when is_list(X) ->
    {seq, fun () -> {list, X} end}.

string(X) ->
    % primitive ToString
    io_lib:format("~p", [X]).

fst(T) -> element(1, T).
snd(T) -> element(2, T).

box(X) -> X.
unbox(X) -> X.

byte(N) when is_number(N) ->
    trunc(N) rem 256;
byte(S) when is_list(S) ->
    case list_to_integer(S) of
        N when N =< 256 ->
            N;
        _ -> % too big
            throw(overflow_exception)
    end.

failwith(M) ->
    throw({exception, M}).

raise(E) ->
    throw(E).

reraise() ->
    % pick exception out of process dictionary
    case get(last_exception) of
        undefined -> throw(reraise);
        E -> throw(E)
    end.
