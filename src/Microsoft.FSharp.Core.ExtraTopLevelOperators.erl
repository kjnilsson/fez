-module('Microsoft.FSharp.Core.ExtraTopLevelOperators').
-compile(export_all).

printfn(S) ->
    io:format(S, []).
% decl MemberOrFunctionOrValue
%   (val tryF,[[val unitVar0]],
%    TryFinally
%   (Application
%   (Let
%   ((val clo1,
%     Call
%   (None,val sprintf,[],
%    [type Microsoft.FSharp.Core.string -> Microsoft.FSharp.Core.string],
%    [Coerce
%   (type Microsoft.FSharp.Core.Printf.StringFormat<(Microsoft.FSharp.Core.string -> Microsoft.FSharp.Core.string)>,
%    NewObject
%   (member .ctor,
%    [type Microsoft.FSharp.Core.string -> Microsoft.FSharp.Core.string;
%     type Microsoft.FSharp.Core.unit; type Microsoft.FSharp.Core.string;
%     type Microsoft.FSharp.Core.string; type Microsoft.FSharp.Core.string],
%    [Const ("%s",type Microsoft.FSharp.Core.string)]))])),
%    Lambda (val arg10,Application (Value val clo1,[],[Value val arg10]))),
%    [],[Const ("hi",type Microsoft.FSharp.Core.string)]),
%    Call
%   (None,val printfn,[],[type Microsoft.FSharp.Core.unit],
%    [Coerce
%   (type Microsoft.FSharp.Core.Printf.TextWriterFormat<Microsoft.FSharp.Core.unit>,
%    NewObject
%   (member .ctor,
%    [type Microsoft.FSharp.Core.unit; type System.IO.TextWriter;
%     type Microsoft.FSharp.Core.unit; type Microsoft.FSharp.Core.unit;
%     type Microsoft.FSharp.Core.unit],
%    [Const ("finally",type Microsoft.FSharp.Core.string)]))])))
