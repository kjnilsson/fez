-module(ce).

-compile(export_all).

ast(File) ->
    {ok, Bin} = file:read_file(File),
    {ok, Toks, _} = core_scan:string(binary_to_list(Bin)),
    {pok, Ast} = core_parse:parse(Toks),
    Ast.
