-module(ce).

-compile(export_all).

ast(File) ->
    {ok, Bin} = file:read_file(File),
    {ok, Toks, _} = core_scan:string(binary_to_list(Bin)),
    {ok, Ast} = core_parse:parse(Toks),
    Ast.
