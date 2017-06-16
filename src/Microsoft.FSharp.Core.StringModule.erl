-module('Microsoft.FSharp.Core.StringModule').
-export([
		length/1,
		string/1,
		concat/2,
		exists/2,
		forall/2,
		init/2,
		map/2,
	    mapi/2,
		replicate/2
	]).


%% TODO: at some point should strings be binaries just
%% like in elixir?


string(X) -> X.


%% Creates a new string whose characters are the results of 
%% applying a specified function to each of the characters 
%% of the input string and concatenating the resulting strings.
%% TODO collect : (char → string) → string → string

%% Returns a new string made by concatenating the given strings with a separator.
%% concat : string → seq<string> → string
concat(Sep, StrList) ->
	string:join(StrList, Sep).

%% Tests if any character of the string satisfies the given predicate.
%% exists : (char → bool) → string → bool
exists(Pred, Str) ->
    lists:any(Pred, Str).

%% Tests if all characters in the string satisfy the given predicate.
%% forall : (char → bool) → string → bool
forall(Pred, Str) ->
	lists:all(Pred, Str).

%% Creates a new string whose characters are the results of applying 
%% a specified function to each index and concatenating the resulting strings.
%% init : int → (int → string) → string
init_(FinalIdx, FinalIdx, _) ->
	"";
init_(Idx, FinalIdx, Fun) ->
	string:concat(Fun(Idx), init_(Idx+1, FinalIdx, Fun)).
init(Idx, Fun) ->
	lists:flatten(init_(0, Idx, Fun)).

%% Applies a specified function to each character in the string.
%% iter : (char → unit) → string → unit
%% TODO: cannot mutate..
% iter(Fun, Str) ->
% 	lists:map(Fun, Str).

%% Applies a specified function to the index of each character in the string and the character itself.
%% iteri : (int → char → unit) → string → unit
%% TODO: cannot mutate..
% iteri_(_, "", _) ->
% 	"";
% iteri_(Idx, [First|Rest], Fun) ->
% 	string:concat([Fun(Idx, First)], iteri_(Idx+1, Rest, Fun)).
% iteri(Fun, Str) ->
% 	iteri_(0, Str, Fun).

%% length : string → int
length(S) when is_list(S) ->
    erlang:length(S).

%% Creates a new string whose characters are the results of 
%% applying a specified function to each of the characters of the input string.
%% map : (char → char) → string → string
map(Fun, Str) ->
	lists:flatten(lists:map(Fun, Str)).

%% Creates a new string whose characters are the results of 
%% applying a specified function to each character and index of the input string.
%% TODO mapi : (int → char → char) → string → string
mapi_(_, [], _) ->
	[];
mapi_(Idx, [First|Rest], Fun) ->
	string:concat([Fun(Idx, First)], mapi_(Idx+1, Rest, Fun)).
mapi(Fun, Str) ->
	lists:flatten(mapi_(0, Str, Fun)).

%% Returns a string by concatenating a specified number of instances of a string.
%% replicate : int → string → string
%% erl --> copies(String, Number) -> Copies
replicate(Num, Str) ->
	string:copies(Str, Num).


