-module('Microsoft.FSharp.Core.Operators').
-compile({no_auto_import,['not'/1]}).

-export([
         %op_Bang/1, (!)
         op_Modulus/2,
         % &&& bitwise AND
         % op_Multiply/2, % (*)
         op_ColonEquals/2, % update ref value
         op_ComposeRight/2,
         op_ComposeLeft/2,
         op_Dereference/1,
         op_PipeLeft/2,
         op_PipeLeft2/3,
         op_PipeLeft3/4,
         op_PipeRight/2,
         op_PipeRight2/3,
         op_PipeRight3/4,
         op_Range/2,
         op_RangeStep/3,
         op_Division/2,
         op_Append/2,
         op_GreaterThanOrEqual/2,
         op_LessThanOrEqual/2,
         'abs'/1,
         %acos/1,
         %asin/1,
         %atan/1,
         %atan2/2,
         box/1,
         byte/1,
         ceil/1,
         %char/1,
         %compare/2,
         %cos/1,
         %cosh/1
         %decimal/1,
         %decr/1,
         %defaultArg/2,
         %enum/1,
         %exit/1,
         %exp/1,
         %Failure/1,
         failwith/1,
         %float/1,
         floor/1,
         fst/1,
         hash/1,
         id/1,
         ignore/1,
         %incr/1,
         %infinity/0,
         %infinityf/0,
         %int/1,
         %int16/1,
         %int32/1,
         %int64/1,
         %invalidArg/2,
         %invalidOp/2,
         %limitedHash/1,
         %lock/0,
         %log/1,
         %log10/1,
         'max'/2,
         'min'/2,
         %nan/0,
         %nanf/0,
         %nativeint/1,
         'not'/1,
         %nullArg/1,
         %pown/2,
         raise/1,
         ref/1,
         reraise/0,
         'round'/1,
         %sbyte/1,
         seq/1,
         %sign/1,
         %sin/1,
         %sinh/1,
         %sizeof/0,
         snd/1,
         %sqrt/1,
         %stderr/0,
         %stdin/0,
         %stdout/0,
         string/1,
         %tan/1,
         %tanh/1,
         %truncate/1,
         %typedefof/0,
         %typeof/0,
         %uint16/1,
         %uint32/1,
         %uint64/1,
         %unativeint/1,
         unbox/1
         %using/2
        ]).

-type ref() :: {ref, reference()}.



% ref setter
op_ColonEquals({ref, Key}, Value) ->
    put(Key, Value),
    unit.

% ref getter
op_Dereference({ref, Key}) ->
    get(Key).

op_ComposeRight(F,G) ->
    fun (X) -> G(F(X)) end.

op_ComposeLeft(G, F) ->
    fun (X) -> G(F(X)) end.

op_PipeLeft(F, A) -> F(A).
op_PipeLeft2(F, A, B) -> F(A, B).
op_PipeLeft3(F, A, B, C) -> F(A, B, C).

op_PipeRight(A, F) -> F(A).
op_PipeRight2(A, B, F) -> F(A, B).
op_PipeRight3(A, B, C, F) -> F(A, B, C).

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


abs(N) ->
    erlang:abs(N).

box(X) -> X.

byte(N) when is_number(N) ->
    trunc(N) rem 256;
byte(S) when is_list(S) ->
    case list_to_integer(S) of
        N when N =< 256 ->
            N;
        _ -> % too big
            throw(overflow_exception)
    end.

ceil(X) when X < 0 ->
    erlang:trunc(X);
ceil(X) ->
    T = erlang:trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T + 1
    end.

failwith(M) ->
    throw({exception, M}).

floor(X) when X < 0 ->
    T = erlang:trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T - 1
    end;
floor(X) ->
    erlang:trunc(X).

fst(T) ->
    element(1, T).

hash(T) ->
    erlang:phash2(T).

id(X) -> X.

ignore(_X) -> ok.

max(A, B) ->
    erlang:max(A, B).

min(A, B) ->
    erlang:min(A, B).

'not'(B) -> not B.

raise(E) ->
    throw(E).


-spec ref(term()) -> ref().
ref(V) ->
    R = make_ref(),
    put(R, V),
    {ref, R}.



reraise() ->
    % pick exception out of process dictionary
    case get(last_exception) of
        undefined -> throw(reraise);
        E -> throw(E)
    end.

round(N) ->
    erlang:round(N).

seq(S) ->
    'Microsoft.FSharp.Collections.SeqModule':seq(S).

string(X) ->
    % primitive ToString
    io_lib:format("~p", [X]).

snd(T) ->
    element(2, T).

unbox(X) -> X.
