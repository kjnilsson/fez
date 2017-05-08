module Fez.Compiler

open System
open System.Reflection
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.SourceCodeServices.BasicPatterns

module B = BasicPatterns

module Map =
    let merge m1 m2 =
        Map.fold (fun s k v -> Map.add k v s) m1 m2

[<AutoOpen>]
module Util =
    let (|FullPath|) file = Path.GetFullPath file

    let (++) = Array.append
    let run = Async.RunSynchronously
    let (|Item|_|) = Map.tryFind

    let (|FileExists|_|) f path =
        let path = f path
        if File.Exists path then Some path else None
    // compiler stuff
    (* let localPath = Path.GetDirectoryName(typeof<TypeInThisAssembly>.GetTypeInfo().Assembly.Location) *)
    let compilerArgs () =
        let fsharpCoreLib = typeof<Microsoft.FSharp.Core.MeasureAttribute>.GetTypeInfo().Assembly.Location
        let fezCoreLib = typeof<Fez.Core.Pid>.GetTypeInfo().Assembly.Location
        let systemCoreLib = typeof<System.Object>.GetTypeInfo().Assembly.Location
        let sysPath = Path.GetDirectoryName(systemCoreLib)
        let sysLib name = Path.Combine(sysPath, name + ".dll")
        (* let localLib name = Path.Combine(localPath, name + ".dll") *)
        let resolve ref =
            match ref with
            | FileExists sysLib path -> path
            (* | FileExists localLib path -> path *)
            | ref -> failwithf "Cannot locate reference %s" ref
        [|
            // "--define:DEBUG"
            "--noframework"
            "--nologo"
            "--simpleresolution"
            "--nocopyfsharpcore"
            "--warn:3"
            "--fullpaths"
            "--flaterrors"
            "--target:library"
            "--targetprofile:netcore"
            "-r:" + systemCoreLib
            "-r:" + resolve "mscorlib"
            "-r:" + resolve "System.Collections"
            "-r:" + resolve "System.Diagnostics.Debug"
            "-r:" + resolve "System.IO"
            "-r:" + resolve "System.Reflection"
            "-r:" + resolve "System.Runtime"
            "-r:" + resolve "System.Runtime.Numerics"
            "-r:" + resolve "System.Threading"
            "-r:" + resolve "System.Threading.Tasks"
            "-r:" + resolve "System.Text.RegularExpressions"
            "-r:" + fsharpCoreLib
            "-r:" + fezCoreLib
        |]

    let projectOptions (checker: FSharpChecker) file =
        {ProjectFileName = "Test"
         ProjectFileNames = [||]
         OtherOptions =
             [|"-o:Test.dll"; "-a"; file|]
             ++ compilerArgs ()
         ReferencedProjects = [||]
         IsIncompleteTypeCheckEnvironment = false
         UseScriptResolutionRules = false
         LoadTime = DateTime.Now
         UnresolvedReferences = None
         OriginalLoadReferences = []
         ExtraProjectInfo = None}


[<AutoOpen>]
module Compiler =

    let rec nonAbbreviatedType (t: FSharpType) =
        if t.IsAbbreviation then
            nonAbbreviatedType t.AbbreviatedType
        else t

    let inline (|NonAbbreviatedType|) (t: FSharpType) =
        nonAbbreviatedType t

    let (|TypeDefinition|_|) (NonAbbreviatedType t) =
        if t.HasTypeDefinition then
            Some t.TypeDefinition
        else None

    let (|IsFSharpList|_|) =
        function
        | TypeDefinition tdef as t
            when tdef.TryFullName =
                Some "Microsoft.FSharp.Collections.FSharpList`1" ->
                    Some t
        | _ -> None

    let (|IsCase|_|) caseName (c : FSharpUnionCase) =
        if c.CompiledName = caseName then
            Some caseName
        else None

    let (|IsCtor|_|) (c : FSharpMemberOrFunctionOrValue) =
        if c.CompiledName = ".ctor" then
            Some c
        else None

    let (|CaseName|) (c : FSharpUnionCase) =
        c.CompiledName |> cerl.Atom

    let (|IsField|_|) fieldName (c : FSharpField) =
        if c.Name = fieldName then
            Some fieldName
        else None

    let check (checker : FSharpChecker) options (FullPath file) fileContents =
        let res = checker.ParseAndCheckProject options |> run
        if res.HasCriticalErrors then
            failwithf "Errs %A" res.Errors
        res

    let fezUnit =
        cerl.Exp (cerl.Constr (cerl.Lit (cerl.LAtom (cerl.Atom "fez_unit"))))

    let (|IsFezUnit|_|) e =
        match e with
        | cerl.Exp (cerl.Constr (cerl.Lit (cerl.LAtom (cerl.Atom "fez_unit")))) ->
            Some e
        | _ -> None

    let stripFezUnit =
        function
        | [IsFezUnit e] -> []
        | args -> args

    let filterUnitVars =
        List.choose (fun (x : FSharpMemberOrFunctionOrValue) ->
            if x.FullName = "unitVar0" then None
            else Some x.FullName)

    let (|Parameters|) (pgs : FSharpMemberOrFunctionOrValue list list) =
        pgs
        |> List.fold List.append []
        |> filterUnitVars

    let inspectT (t: FSharpType) =
        t.TypeDefinition.UnionCases

    let altExpr = cerl.altExpr

    let boolPat tf = (cerl.Pat (cerl.PLit (cerl.LAtom (cerl.Atom tf))))

    let mkName (name : string) num =
        if Char.IsUpper name.[0] then
            sprintf "%s%i" name num
        else
            sprintf "_%s%i" name num

    let safeVar incr nameMap (x : cerl.Var) : (cerl.Var * Map<cerl.Var, int>) =
        match incr, nameMap with
        | false, Item x num ->
            mkName x num, nameMap
        | true, Item x num ->
            let num' = num + 1
            mkName x num', Map.add x num' nameMap
        | _ ->
            mkName x 0, Map.add x 0 nameMap

    let varExps name =
        cerl.Constr (cerl.Var name) |> cerl.Exp

    let uniqueName nm =
        // TODO: append some random stuff to reduce the chance of name collisions
        safeVar true nm "fez"

    let foldNames nm f xs =
        let xs, nm = List.fold (fun (xs, nm) x ->
                                    let x, nm = f nm x
                                    x :: xs, nm) ([], nm) xs
        List.rev xs, nm

    let constr x =
        cerl.Exp (cerl.Constr x)

    let litAtom = cerl.litAtom

    let litInt i =
        cerl.Lit (cerl.LInt i)

    let litString s =
        cerl.Lit (cerl.LString s)

    let modCall left right exps =
        cerl.ModCall ((left, right), exps)

    let apply f args =
        cerl.App (f, args)

    let lambda args expr =
        cerl.Lambda (args, expr)

    let mkLet v a expr =
        cerl.Let (([v], a), expr)

    let mkFunction name arity =
        cerl.Function (cerl.Atom name, arity)

    let funDef f (expr) =
        cerl.FunDef (cerl.Constr f, cerl.Constr expr)

    let annLAtom n = litAtom n |> constr
    let erlang = litAtom "erlang" |> constr
    let notEquals = litAtom "/=" |> constr
    let equals = litAtom "=:=" |> constr
    let fez = litAtom "fez" |> constr

    let (|Intr2Erl|_|) (f: FSharpMemberOrFunctionOrValue) =
        match f.LogicalName with
        | "op_Multiply" -> Some "*"
        | "op_Addition" -> Some "+"
        | "op_Subtraction" -> Some "-"
        | "op_LessThan" -> Some "<"
        | "op_GreaterThan" -> Some ">"
        | "op_Equality" -> Some "=:="
        | _ -> None

    let castLiteral (o: obj) =
        match o with
        | :? int as i -> cerl.LInt i |> Some
        | :? float as f -> cerl.LFloat f |> Some
        | :? float32 as f -> cerl.LFloat (float f) |> Some
        | :? char as c -> cerl.LChar c |> Some
        | :? string as s -> cerl.LString s |> Some
        | x -> None

    let (|Lit|_|) = castLiteral

    let tupleGet idx e =
        let element = litAtom "element" |> constr
        let idx = idx+1 |> cerl.LInt
        modCall erlang element [cerl.Lit idx |> constr; e]

    let toLowerString (o:obj) =
        o.ToString().ToLower()

    //TODO: should we consult the FSharpType as well?
    let mapConst (o : obj) =
        match o with
        | :? int as i -> litInt i
        | :? string as s -> litString s
        | :? bool as b -> litAtom (toLowerString b)
        | null -> //unit?
            litAtom "fez_unit" //Special casing a value here for unit for now
        | x -> failwithf "mapConst: not impl %A" x

    let groupPatterns groupBy patterns =
        patterns
        |> List.groupBy groupBy
        |> List.map (fun (k, v) -> (k, List.map snd v))

    let mergePatterns patterns =
        (* printfn "grouped patterns %A" grouped *)
        patterns
        |> List.map (fun (k, vs) ->
                let s = List.head vs
                let rest = List.tail vs
                k, List.fold (fun (ps, gs, ctxs)
                               (p, g, ctx) ->
                                    cerl.mergePat (ps, p),
                                        cerl.mergeGuards (gs, g),
                                            ctx)
                           s rest)

    // flatten nested single parameter lambdas
    // this will reverse the arguments but that is typically ok for
    // a first class fun in erlang
    let rec flattenLambda parms l =
        match parms, l with
        | _, cerl.Exp (cerl.Constr (cerl.Lambda ([v] , exps))) ->
            flattenLambda (v :: parms) exps
        | [], _ -> l
        | _, _ ->
            cerl.Exp (cerl.Constr (cerl.Lambda (parms, l)))

    let (|ExprType|_|) ts (e: FSharpExpr) =
        if e.Type.TypeDefinition.LogicalName = ts then Some e
        else None

    let (|IsMemberOn|_|) t (f: FSharpMemberOrFunctionOrValue) =
        if f.IsMember && f.EnclosingEntity.LogicalName = t then
            Some f
        else None

    let (|IsModuleMemberOn|_|) t (f: FSharpMemberOrFunctionOrValue) =
        if f.IsModuleValueOrMember && f.EnclosingEntity.LogicalName = t then
            Some f
        else None

    let (|ShimmedCall|_|) (f: FSharpMemberOrFunctionOrValue) =
        let modules =
            ["Microsoft.FSharp.Collections.ListModule"
             "Microsoft.FSharp.Core.Operators"
             "Fez.Core"
            ]
        if f.IsModuleValueOrMember &&
           List.contains f.EnclosingEntity.FullName modules then
            Some f
        else None

    let (|LogicalName|_|) t (f: FSharpMemberOrFunctionOrValue) =
        if f.LogicalName = t then Some ()
        else None

    let (|IsUnitArg|_|) (f : FSharpMemberOrFunctionOrValue) =
        if f.FullType.ToString() = "type Microsoft.FSharp.Core.unit" then Some f
        else None

    let (|IsUnit|_|) (f : FSharpType) =
        if f.ToString() = "type Microsoft.FSharp.Core.unit" then Some f
        else None

    let rec translateCall nm callee
                          (f : FSharpMemberOrFunctionOrValue)
                          (argTypes: FSharpType list)
                          (exprs : FSharpExpr list) : (cerl.Exps * Map<string, int>) =
        match callee, f, exprs with
        //special case mapping + on a string to ++
        | _, Intr2Erl "+", ExprType "string" _ :: _ ->
            let stringAppend = litAtom "++" |> constr
            let args, nm = foldNames nm processExpr exprs
            modCall erlang stringAppend args |> constr, nm
        | _, Intr2Erl x, _ ->
            let op = litAtom x |> constr
            let args, nm = foldNames nm processExpr exprs
            modCall erlang op args |> constr, nm
        | Some callee, LogicalName "get_Length"
                       & (IsMemberOn "String" _ | IsMemberOn "List`1" _), _ ->
            let length = litAtom "length" |> constr
            let arg, nm = processExpr nm callee
            // string length wont have any args
            // List.length has one arg - unit - ignoring it here
            modCall erlang length [arg] |> constr, nm
        | None, LogicalName "length" & IsModuleMemberOn "StringModule" _, _ ->
            let length = litAtom "length" |> constr
            let args, nm = foldNames nm processExpr exprs
            modCall erlang length args |> constr, nm
        (* | None, IsModuleMemberOn "Microsoft.FSharp.Core.ListModule" name, _ -> // FSharpCore module call *)
        | None, ShimmedCall f, _ -> // FSharpCore module call
            let name = f.LogicalName
            let eeFullName = f.EnclosingEntity.FullName
            let m = litAtom eeFullName |> constr
            let f = litAtom name |> constr
            let args, nm = foldNames nm processExpr exprs
            // remove fez_unit
            let args = args |> stripFezUnit |> List.map (flattenLambda [])
            modCall m f args |> constr, nm
        | None, LogicalName "printfn" _, [e] ->
            let io = annLAtom "io"
            let format = annLAtom "format"
            let arg, nm = processExpr nm e
            let argsVar, nm = safeVar true nm "fez"
            let argsArg = cerl.Var argsVar |> constr
            let call = modCall io format [arg; argsArg] |> constr
            // inspect string to see how many actual args there are?
            lambda [argsVar] call |> constr, nm
        | callee, f, e -> //apply to named function (local)
            let name = f.LogicalName
            (* printfn "translatecall %s %A %A" name argTypes exprs *)
            // TODO this probably wont always work
            let funName = litAtom name
            let args, nm =
                exprs
                |> foldNames nm processExpr
            (* let args = *)
            (*     args |> List.zip argTypes *)
            (*     |> List.map (fun (t, a) -> *)
            (*         match t with *)
            (*         | IsUnit t -> fezUnit *)
            (*         | _ -> a) *)
            let args = args |> stripFezUnit
            let numArgs = List.length args
            let func = mkFunction name numArgs |> cerl.Fun |> constr
            let app = apply func args
            constr app,nm
        | _, x, _ ->  failwithf "not implemented %A" x

    and processDT nm (expsLookup : Map<int, FSharpMemberOrFunctionOrValue list * FSharpExpr>)  expr =
        match expr with
        | B.IfThenElse(fi, neht, esle) ->
            let ifExps, nm = processExpr nm fi
            let thenExpr, nm = processDT nm expsLookup neht
            let elseExpr, nm = processDT nm expsLookup esle
            let a1 = altExpr (boolPat "true", cerl.defaultGuard, constr thenExpr)
            let a2 = altExpr (boolPat "false", cerl.defaultGuard, constr elseExpr)
            cerl.Case(ifExps, [a1;a2]), nm
        | B.DecisionTreeSuccess(i, []) ->
            let mfvs, expr = expsLookup.[i]
            let e, nm = processExpr nm expr
            match e with
            | cerl.Exp (cerl.Constr e) -> e, nm
            | _ -> failwith "no"
        | B.DecisionTreeSuccess(i, valueExprs) ->
            let mfvs, expr = expsLookup.[i]
            let mfvs = mfvs |> List.map (fun v -> v.CompiledName)
            // process expression and wrap in multi let
            let assignments, mn =
                List.zip mfvs valueExprs
                |> List.fold (fun (agg, nm) (v, e) ->
                                let v, nm = safeVar false nm v
                                let e, nm = processExpr nm e
                                ((v, e) :: agg), nm) ([], nm)

            let e, _ = processExpr nm expr

            let vls = List.map fst assignments
            let es = List.choose (fun (_, e) ->
                                    match e with
                                    | cerl.Exp ae -> Some ae
                                    | _ -> None) assignments
            cerl.Let ((vls, cerl.Exps (cerl.Constr es)), e), nm
        | e -> failwithf "processDT unexpected %A" e


    and processExpr nm (expr : FSharpExpr) : (cerl.Exps * Map<string, int>) =
        let element nm idx e =
            let el = litAtom "element" |> constr
            let e, nm = processExpr nm e
            let idx = cerl.LInt idx
            modCall erlang el [cerl.Lit idx |> constr; e] |> constr, nm

        match expr with
        | B.UnionCaseTest (e, IsFSharpList t, IsCase "Cons" c) ->
            let a1, nm = processExpr nm e
            let a2 = cerl.Lit (cerl.LNil) |> constr
            modCall erlang notEquals [a1; a2] |> constr, nm
        | B.UnionCaseTest (e, IsFSharpList t, IsCase "Empty" c) ->
            let a1, nm = processExpr nm e
            let a2 = cerl.Lit (cerl.LNil) |> constr
            modCall erlang equals [a1; a2] |> constr, nm
            //Cons cell without any name bindings
        | B.UnionCaseTest (e, t, CaseName name) ->
            let left, nm = element nm 1 e
            let right = cerl.Lit (cerl.LAtom name) |> constr
            modCall erlang equals [left; right] |> constr, nm
    // any other DUs turn them into {'CaseName\, _, _, _} patterns
        | B.Call (callee, f, _, argTypes, expressions) ->
            (* printfn "Call expr %A %A %A" expr f expressions *)
            translateCall nm callee f argTypes expressions
        | B.Value v ->
            let v', nm = safeVar false nm v.LogicalName
            cerl.Var v' |> constr, nm
        | B.Const (o, t) ->
            mapConst o |> constr, nm
        | B.NewTuple (fsType, args) ->
            let args, nm = foldNames nm processExpr args
            cerl.Tuple args |> constr, nm
        | B.TupleGet (fsType, idx, e) ->
            let idx = idx+1
            element nm idx e
        | B.NewUnionCase(fsType, fsUnionCase, argExprs) as e ->
            let unionTag = fsUnionCase.CompiledName |> litAtom |> constr
            let args, nm = foldNames nm processExpr argExprs
            cerl.Tuple (unionTag :: args) |> constr, nm
            (* failwithf "NewUnionCase not impl %A" e *)
        | B.Let ((v, B.Call (_, (LogicalName "receive" as m), _t, [t], _)), expr) as r ->
            // generate basic structural case to for the DU type
            // then generate standard if then else
            let cases = t.TypeDefinition.UnionCases
            let alias, nm = uniqueName nm
            let mkAliasP p = cerl.PAlias (cerl.Alias (alias, p))

            let alts =
                cases
                |> Seq.map (fun c ->
                    let tag = cerl.PLit (cerl.LAtom (cerl.Atom c.Name))
                    let fields =
                        c.UnionCaseFields
                        |> Seq.map (fun cf -> cerl.PVar "_")
                        |> Seq.toList
                    let pat = cerl.PTuple (tag :: fields) |> mkAliasP
                    cerl.Constr (cerl.Alt (cerl.Pat pat, cerl.defaultGuard, constr (cerl.Var alias)))
                )
                |> Seq.toList
            let infinity =
                let expiry = litAtom "infinity" |> constr
                let body = litAtom "true" |> constr
                cerl.TimeOut (expiry, body)
            let receive = cerl.Receive (alts, infinity) |> constr
            let n, nm = safeVar true nm v.LogicalName
            let letExps, nm = processExpr nm expr
            mkLet n receive letExps |> constr, nm
        | B.Let ((v, e), expr) as l ->
            // ignore names introduced in the variable assignment expression
            let ass, _ = processExpr nm e
            let v', nm = safeVar true nm v.LogicalName
            let next, nm = processExpr nm expr
            mkLet v' ass next |> constr, nm
        | B.IfThenElse (fi, neht, esle) as ite ->
            //plain if then else without decision tree
            (* printfn "if %A\r\nthen %A\r\nelse %A" fi neht esle *)
            let ifExps, nm = processExpr nm fi
            let thenExpr, nm = processExpr nm neht
            let elseExpr, nm = processExpr nm esle
            let a1 = altExpr (boolPat "true", cerl.defaultGuard, thenExpr)
            let a2 = altExpr (boolPat "false", cerl.defaultGuard, elseExpr)
            cerl.Case(ifExps, [a1;a2]) |> constr, nm
        | B.DecisionTree (B.IfThenElse (fi, _, _) as ite, branches) as tree ->
            (* printfn "ite %A" ite *)
            let l = List.mapi (fun i x -> i, x) branches |> Map
            let e, nm = processDT nm l ite
            constr e, nm
        | B.FSharpFieldGet (Some e, t, fld) ->
            // TODO when would the expr be None here
            let tupleIndex =
                t.TypeDefinition.FSharpFields
                |> Seq.findIndex ((=) fld)
                |> (+) 1
            let e, nm = processExpr nm e
            tupleGet tupleIndex e |> constr, nm
        | B.NewRecord (t, args) ->
            let args, nm = foldNames nm processExpr args
            //type to atom
            let recordName =
                litAtom t.TypeDefinition.LogicalName |> constr
            cerl.Tuple (recordName :: args) |> constr, nm
        | B.UnionCaseGet(value, IsFSharpList fsType, IsCase "Cons" uCas,
                         IsField "Head" fld) ->

             let hd = litAtom "hd" |> constr
             let e, nm = processExpr nm value
             modCall erlang hd [e] |> constr, nm
        | B.UnionCaseGet(value, IsFSharpList fsType, IsCase "Cons" uCas,
                         IsField "Tail" fld) ->
             let hd = litAtom "tl" |> constr
             let e, nm = processExpr nm value
             modCall erlang hd [e] |> constr, nm
        | B.UnionCaseGet(e, t, c, f) ->
            (* printfn "v %A t %A c %A f %A" e t c.UnionCaseFields f *)
            // turn these into element/2 calls
            let idx =
                c.UnionCaseFields
                |> Seq.findIndex ((=) f)
            let element = litAtom "element" |> constr
            let e, nm = processExpr nm e
            let idx = idx+2 |> cerl.LInt
            modCall erlang element [cerl.Lit idx |> constr; e] |> constr, nm
        | B.Coerce(a, e) ->
            processExpr nm e
        | B.NewObject(IsCtor m, types, exprs) ->
            (* printfn "m %A" m.CompiledName *)
            let expss, nm = foldNames nm processExpr exprs
            let expss = expss
                        |> List.choose (function
                                        | cerl.Exp ae -> Some ae
                                        // should be a fold so we can use all Exps
                                        (* | cerl.Exps ae -> Some ae *)
                                        | _ -> None)
            cerl.Exps (cerl.Constr expss), nm
        // horrendously specific match to intercept printfn and sprintf
        | B.Application (B.Let ((_, B.Call (None, (LogicalName "printfn" | LogicalName "sprintf" as p), _, _,
                                            [B.Coerce (_, B.NewObject (_, _, [B.Const (:? string as str, _)]))])), _letBody),
                         _types, args) ->
            let format = annLAtom "format"
            // primitive format converion
            let str = str.Replace("%s", "~s")
                         .Replace("%i", "~b")
                         .Replace("%A", "~p")
            let io, str =
                match p.LogicalName with
                | "printfn" ->
                    let str = str + "~n" //add newline as io:format does not
                    annLAtom "io", str
                | "sprintf" ->
                    annLAtom "io_lib", str
                | _ -> failwith "unexpected"
            let arg1 = mapConst str |> constr
            let args, nm = foldNames nm processExpr args
            let args = [arg1; cerl.List (cerl.L args) |> constr]
            modCall io format args |> constr, nm
        | B.Application (target, _types, args) ->
            (* printfn "app target: %A args: %A" target args *)
            // if the target is not a plain value or a function we
            // may not be able to process it inline and thus need to wrap it
            // in a Let
            match processExpr nm target with
            | cerl.Exp (cerl.Constr (cerl.Var _ | cerl.Fun _ )) as t, nm ->
                // we're cool the target is just a var or fun - we can inline
                // TODO: literals?
                let args, nm = foldNames nm processExpr args
                apply t args |> constr, nm
            | t, nm ->
                //the target is something more complex and needs to be
                //wrapped in a Let
                let name, nm = uniqueName nm
                let app, nm =
                    let args, nm = foldNames nm processExpr args
                    apply (varExps name) args |> constr, nm
                (* printfn "APP %A" app *)
                mkLet name t app |> constr, nm
        | B.Sequential(first, second) ->
            let f, nm = processExpr nm first
            let s, nm = processExpr nm second
            cerl.Seq (f, s) |> constr, nm
        | B.Lambda (IsUnitArg p, expr) ->
            (* printfn "Lambda!! %A %A" p (p.FullType.ToString()) *)
            let unitName, nm = safeVar true nm p.LogicalName
            let body, nm = processExpr nm expr
            // wrap body in let so that unit arg is mapped to fez_unit
            let body = mkLet unitName fezUnit body |> constr
            cerl.Lambda ([], body) |> constr, nm
        | B.Lambda (p, expr) ->
            (* printfn "Lambda! %A %A" p (p.FullType.ToString()) *)
            let v, nm = safeVar true nm p.LogicalName
            let body, nm = processExpr nm expr
            cerl.Lambda ([v], body) |> constr, nm
        | x -> failwithf "not implemented %A" x


    let processModDecl decl =
        (* printfn "decl %A" decl *)
        match decl with
        | MemberOrFunctionOrValue(memb, Parameters ps, expr)
            when memb.IsModuleValueOrMember && not memb.IsCompilerGenerated ->
            let nm = Map.empty
            let args, nm = foldNames nm (safeVar true) ps
            let e, nm = processExpr nm expr
            let l = lambda args e
            //TODO make function name safe
            let f = mkFunction memb.LogicalName (List.length ps)
            Some (f, funDef f l)
        | Entity(ent, declList) when ent.IsFSharpRecord ->
            None
        | Entity(ent, declList) when ent.IsFSharpUnion ->
            None
        |  MemberOrFunctionOrValue(x, _, _) ->
        (* printfn "cannot process %A" x.LogicalName *)
            None
        | x -> failwithf "cannot process %A" x


    // built in simple operations that aren't available in erlang
    // are inlined as private functions
    // this makes translation easier
    // bonus: apply may be faster than modcall
    // TODO: only inline those that are actually used
    let defaultFunDefs =
        [
            (* cerl.op_ComposeRight *)
            (* cerl.op_ComposeLeft *)
        ]

    let processDecl decl =
      match decl with
      | Entity(ent, implFileDecls) when ent.IsFSharpModule ->
          let (funs, funDefs) =
              implFileDecls
              |> List.choose processModDecl
              |> List.unzip
          cerl.Module (cerl.Atom ent.LogicalName, funs, [],
                       funDefs @ defaultFunDefs)
      | InitAction(expr) ->
          failwithf "Module values (InitActions) are not supported as there is no equivalent in erlang.\r\nMake it a function instead.\r\n%A" expr
      | Entity(ent, declList) ->
          failwithf "cannot process record %+A" ent.TryGetMembersFunctionsAndValues
      | x -> failwithf "cannot process %+A" x

