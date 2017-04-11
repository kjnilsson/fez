#!/usr/bin/env escript
% vi: ft=erlang

ast(File) ->
    {ok, Bin} = file:read_file(File),
    {ok, Toks, _} = core_scan:string(binary_to_list(Bin)),
    {ok, Ast} = core_parse:parse(Toks),
    Ast.

fmt(File) ->
    Ast = ast(File),
    core_pp:format(Ast).

main(["fmt", File]) ->
    io:format("~s~n", [fmt(File)]);
main(["ast", File]) ->
    io:format("~p~n", [ast(File)]).

