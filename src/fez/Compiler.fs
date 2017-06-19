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
    let (|ToList|) = Seq.toList

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

    let projectOptions (checker: FSharpChecker) files =
        {ProjectFileName = "Test"
         ProjectFileNames = [||]
         OtherOptions =
             [|"-o:Test.dll"; "-a"|]
             ++ files
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

    let (|IsFSharpOption|_|) =
        function
        | TypeDefinition tdef as t
            when tdef.TryFullName =
                Some "Microsoft.FSharp.Core.FSharpOption`1" ->
                    Some t
        | _ -> None

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


    (* let (|CaseName|) (c : FSharpUnionCase) = *)
    (*     c.CompiledName |> cerl.Atom *)

    let (|IsField|_|) fieldName (c : FSharpField) =
        if c.Name = fieldName then
            Some fieldName
        else None

    let check (checker : FSharpChecker) options (FullPath file) fileContents =
        let res = checker.ParseAndCheckProject options |> run
        if not (Array.isEmpty res.Errors) || res.HasCriticalErrors then
            failwithf "Errs %A" res.Errors
        res


    let fezUnit =
        cerl.Exp (cerl.Constr (cerl.Lit (cerl.LAtom (cerl.Atom "fez_unit"))))

    let (|IsFezUnit|_|) e =
        match e with
        | cerl.Exp (cerl.Constr (cerl.Lit (cerl.LAtom (cerl.Atom "fez_unit")))) ->
            Some e
        | _ -> None

    let rec stripFezUnit args =
        match args with
        | [IsFezUnit _] -> []
        | args -> args

    let rec stripFezUnits args = [
        match args with
        | IsFezUnit _ :: tail ->
            yield! stripFezUnit tail
        | a :: tail ->
            yield a
            yield! stripFezUnit tail
        | [] -> ()
    ]

    let filterUnitVars =
        List.choose (fun (x : FSharpMemberOrFunctionOrValue) ->
            if x.FullName.StartsWith("unitVar") then None
            else Some x.FullName)

    let (|Parameters|) (pgs : FSharpMemberOrFunctionOrValue list list) =
        let ps = pgs |> List.fold List.append []
        match ps with
        | [x] when x.FullName.StartsWith("unitVar") ->
            []
        | xs ->
            xs |> List.map (fun x -> x.FullName)

    let inspectT (t: FSharpType) =
        t.TypeDefinition.Namespace,
        t.TypeDefinition.CompiledName,
        t.TypeDefinition.LogicalName,
        t.TypeDefinition.FullName

    let altExpr = cerl.altExpr

    let boolPat tf = (cerl.Pat (cerl.PLit (cerl.LAtom (cerl.Atom tf))))

    let mkName (name : string) num =
        // TODO do replacements better
        let name = name
                    .Replace(''', '_')
        if Char.IsUpper name.[0] then
            sprintf "%s%i" name num
        else
            sprintf "_%s%i" name num

    type Ctx =
        { Module: string
          Names : Map<cerl.Var, int>
          Functions : Map<cerl.Var, cerl.Function> }
       with static member init m =
               {Module = m
                Names = Map.empty
                Functions = Map.empty}

    let safeVar incr ({Names = nameMap} as ctx) (x : cerl.Var) : cerl.Var * Ctx =
        match incr, nameMap with
        | false, Item x num ->
            mkName x num, ctx
        | true, Item x num ->
            let num' = num + 1
            mkName x num', {ctx with Names = Map.add x num' nameMap}
        | _ ->
            mkName x 0, {ctx with  Names = Map.add x 0 nameMap}

    let varExps name =
        cerl.Constr (cerl.Var name) |> cerl.Exp

    let uniqueName nm =
        // TODO: append some random stuff to reduce the chance of name collisions
        safeVar true nm "fez"

    let foldNames (ctx : Ctx) f xs =
        let xs, ctx = List.fold (fun (xs, nm) x ->
                                    let x, nm = f nm x
                                    x :: xs, nm) ([], ctx) xs
        List.rev xs, ctx

    let constr x =
        cerl.Exp (cerl.Constr x)

    let unconstr =
        function
        | cerl.Exp (cerl.Constr c) -> c
        | _ -> failwith "not a constr"

    let litAtom = cerl.litAtom

    let litInt i =
        cerl.Lit (cerl.LInt i)

    let litChar s =
        cerl.Lit (cerl.LChar s)

    let litString s =
        cerl.Lit (cerl.LString s)

    let modCall left right exps =
        cerl.ModCall ((left, right), exps)

    let apply f args =
        cerl.App (f, args)

    let lambda args expr =
        cerl.Lambda (args, expr)

    let rec flattenLambda parms l =
        match parms, l with
        | _, cerl.Exp (cerl.Constr (cerl.Lambda ([v] , exps))) ->
            flattenLambda (v :: parms) exps
        | [], _ -> l
        | _, _ ->
            cerl.Exp (cerl.Constr (cerl.Lambda (List.rev parms, l)))

    let mkLet v a expr =
        cerl.Let (([v], a), flattenLambda [] expr)

    let mkFunction ({Functions = funs} as ctx) name arity =
        let f = cerl.Function (cerl.Atom name, arity)
        f, {ctx with Functions = Map.add name f funs}

    let funDef f (expr) =
        cerl.FunDef (cerl.Constr f, cerl.Constr expr)

    let lAtomPat name =
        cerl.PLit (cerl.LAtom (cerl.Atom name))

    let mkType (t:FSharpType) wrap =
        let ln = t.TypeDefinition.LogicalName
        let fn = t.TypeDefinition.FullName
        let fn = fn.Replace(ln, "").TrimEnd('.')
        let m = fn |>  wrap
        let t = ln |>  wrap
        [m;t]

    let mkTypeTag (t: FSharpType) =
        cerl.Tuple (mkType t (litAtom >> constr))

    let mkUnionTag (uc : FSharpUnionCase) =
        uc.Name |> litAtom |> constr

    let mkTypePat (t: FSharpType) =
        let m = t.TypeDefinition.FullName |> lAtomPat
        mkType t lAtomPat |> cerl.PTuple

    let mkUnionPat (uc : FSharpUnionCase) =
        uc.Name |> lAtomPat

    let trueExps = litAtom "true" |> constr
    let falseExps = litAtom "false" |> constr
    let mkAlt p g e =
        cerl.Constr (cerl.Alt (cerl.Pat p, g, e))

    let annLAtom n = litAtom n |> constr
    let erlang = litAtom "erlang" |> constr
    let notEquals = litAtom "/=" |> constr
    let equals = litAtom "=:=" |> constr
    let fez = litAtom "fez" |> constr

    let mkStructuralUnionCasePat (t: FSharpType) (uc: FSharpUnionCase) =
        let typeTag = mkTypePat t
        let caseTag = mkUnionPat uc
        let fields =
            uc.UnionCaseFields
            |> Seq.map (fun cf -> cerl.PVar "_")
            |> Seq.toList
        cerl.PTuple (typeTag :: caseTag :: fields)

    let mkErlangTermCasePat ctx (t: FSharpType) (uc: FSharpUnionCase) =
        let mkTypeCheck (t: FSharpType) name =
            let ln = t.TypeDefinition.LogicalName
            match ln with
            | "int" ->
                cerl.ModCall((erlang, litAtom "is_integer" |> constr), [name]) |> constr
            | "string" ->
                cerl.ModCall((erlang, litAtom "is_list" |> constr), [name]) |> constr
            | x -> failwithf "mkTypeCheck not impl %A" x
        match Seq.toList uc.UnionCaseFields with
        | [] -> //no args match on atom
            uc.Name.ToLower() |> lAtomPat, cerl.defaultGuard
        | [a] ->
            let name, ctx = uniqueName ctx
            let guard = cerl.Guard (mkTypeCheck (a.FieldType) (cerl.Var name |> constr))
            let field = cerl.PVar name
            field, guard
        | fields ->
            // turn field into nested guard type check expression
            let nameChecks, ctx =
                fields
                |> foldNames ctx (fun ctx f ->
                    let name, ctx = uniqueName ctx
                    let check = mkTypeCheck f.FieldType (cerl.Var name |> constr)
                    (name, check), ctx)
            let checks = nameChecks |> List.map snd
            let patterns = nameChecks |> List.map (fst >> cerl.PVar)
            let falseExp = litAtom "false"
            let trueExp = litAtom "true"
            let wrap ifExps thenExps =
                let a1 = altExpr (boolPat "true", cerl.defaultGuard, constr thenExps)
                let a2 = altExpr (boolPat "false", cerl.defaultGuard, constr falseExp)
                cerl.Case(ifExps, [a1;a2])

            let state = wrap (List.head checks) trueExp
            let res =
                List.tail checks
                |> List.fold (fun s c -> wrap c s) state

            cerl.PTuple patterns, cerl.Guard (constr res)

    let put k v =
        let put = litAtom "put" |> constr
        modCall erlang put [k; v]

    let mkTryAfter ctx e after =
        let sucVal, ctx = uniqueName ctx
        let sucExps = cerl.Seq (after, constr (cerl.Var sucVal)) |> constr
        let ofs = [sucVal], sucExps
        let n1, ctx = uniqueName ctx
        let n2, ctx = uniqueName ctx
        let n3, ctx = uniqueName ctx
        let reRaise = cerl.Op (cerl.Atom "raise",
                               [constr (cerl.Var n3); constr (cerl.Var n2)])
        let afterExps = cerl.Seq (after, constr reRaise) |> constr
        let catch = [n1;n2;n3], afterExps
        cerl.Try (e, ofs, catch) |> constr, ctx


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
        | :? int as i -> cerl.LInt (int64 i) |> Some
        | :? float as f -> cerl.LFloat f |> Some
        | :? float32 as f -> cerl.LFloat (float f) |> Some
        | :? char as c -> cerl.LChar c |> Some
        | :? string as s -> cerl.LString s |> Some
        | x -> None

    let (|Lit|_|) = castLiteral

    let tupleGet idx e =
        let element = litAtom "element" |> constr
        let idx = idx+1L |> cerl.LInt
        modCall erlang element [cerl.Lit idx |> constr; e]

    let toLowerString (o:obj) =
        o.ToString().ToLower()

    let mapConst (o : obj) (t: FSharpType) =
        let td = t.TypeDefinition
        match o with
        | :? int as i -> litInt (int64 i)
        | :? int64 as i -> litInt i
        | :? char as c -> litChar c
        | :? string as s -> litString s
        | :? bool as b -> litAtom (toLowerString b)
        | null -> //unit
            litAtom "fez_unit" //Special casing a value here for unit for now
        | x -> failwithf "mapConst: not impl %A" x

    let ioLibFormat nm str t args =
        let io = annLAtom "io_lib"
        let format = annLAtom "format"
        let arg1 = mapConst str t |> constr
        let args = [arg1; cerl.List (cerl.L args) |> constr]
        modCall io format args |> constr, nm
    // flatten nested single parameter lambdas
    // this will reverse the arguments but that is typically ok for
    // a first class fun in erlang
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

    let (|LogicalName|_|) t (f: FSharpMemberOrFunctionOrValue) =
        if f.LogicalName = t then Some ()
        else None

    let (|IsUnitArg|_|) (f : FSharpMemberOrFunctionOrValue) =
        if f.FullType.ToString() = "type Microsoft.FSharp.Core.unit" then Some f
        else None

    let (|IsUnit|_|) (f : FSharpType) =
        if f.ToString() = "type Microsoft.FSharp.Core.unit" then Some f
        else None

    let traitCall name (args : cerl.Exps list) =
        let fezCore = litAtom "Fez.Core" |> constr
        let traitCall = litAtom "trait_call" |> constr
        let instance = args.[0]
        let listArgs = cerl.List(cerl.L args) |> constr
        modCall fezCore traitCall [instance; litAtom name |> constr; listArgs]

    let rec translateCall nm callee
                          (f : FSharpMemberOrFunctionOrValue)
                          (argTypes: FSharpType list)
                          (exprs : FSharpExpr list) : (cerl.Exps * Ctx) =
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
        | Some callee, LogicalName "get_Message"
                       & (IsMemberOn "Exception" _ ), _ ->
            let arg, nm = processExpr nm callee
            let t= callee.Type
            ioLibFormat nm "~p" t [arg]
        | None, f, [e] when f.FullName = "Microsoft.FSharp.Core.Operators.string"
                            && e.Type.TypeDefinition.LogicalName = "string" ->
            //erase ToString on strings
            processExpr nm e
        | callee, f, e when f.EnclosingEntity.FullName = nm.Module
                            || (f.EnclosingEntity.IsFSharpUnion
                                || f.EnclosingEntity.IsFSharpRecord) -> //apply to named function
            let ee = f.EnclosingEntity
            let exprs = match callee with
                        | Some e -> e :: exprs
                        | None -> exprs
            let name =
                if ee.IsFSharpUnion || ee.IsFSharpRecord then
                    //method on type rather than nested module
                    ee.LogicalName + "." + f.LogicalName
                else
                    f.LogicalName
            let funName = litAtom name
            //add callee as first arg if method dispatch
            //best effort only
            let args, nm =
                exprs
                |> foldNames nm processExpr
            let args = args |> stripFezUnit |> List.map (flattenLambda [])
            let numArgs = List.length args
            let func, nm = mkFunction nm name numArgs
            let func = func |> cerl.Fun |> constr
            let app = apply func args
            constr app,nm
        | None, f, _ -> // module call
            let name = f.LogicalName
            let eeFullName = f.EnclosingEntity.FullName
            let m = litAtom eeFullName |> constr
            let f = litAtom name |> constr
            let args, nm = foldNames nm processExpr exprs
            // remove fez_unit
            // flatten any lambda args
            let args = args |> stripFezUnit |> List.map (flattenLambda [])
            modCall m f args |> constr, nm
        | Some o, f, _ ->
            // attempt dispatch on object member by adding the "dispatchee"
            // as the first argument to the function
            let name = f.LogicalName
            let args, nm = foldNames nm processExpr (o :: exprs)
            // flatten any lambda args
            let stripFezUnit args =
                // first arg is the dispatch object
                // so we need special version here
                match args with
                | [o; IsFezUnit _] -> [o]
                | args -> args
            let args = args |> stripFezUnit |> List.map (flattenLambda [])
            let fe = f.EnclosingEntity
            if fe.IsInterface then
                // use trait call for dispatch on inteface as it will use the
                // embedded type info (if available) to dispatch the call to the right function
                // Else we'd need a function of format: 'Module:InterfaceType.Member'
                traitCall name args |> constr, nm
            else
                let m =
                    let t = nonAbbreviatedType o.Type
                    // recurse back to non abbr type name
                    t.TypeDefinition.FullName |> litAtom |> constr
                let f = litAtom name |> constr
                modCall m f args |> constr, nm
        | x ->
            failwithf "not implemented %A" x

    and processDT nm (expsLookup : Map<int, FSharpMemberOrFunctionOrValue list * FSharpExpr>)  expr =
        match expr with
        | B.Let ((v, e), expr) ->
            // ignore names introduced in the variable assignment expression
            let ass, _ = processExpr nm e
            let ass = flattenLambda [] ass
            let v', nm = safeVar true nm v.LogicalName
            let next, nm = processDT nm expsLookup expr
            mkLet v' ass (constr next), nm
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


    and processExpr nm (expr : FSharpExpr) : (cerl.Exps * Ctx) =
        let element nm idx e =
            let el = litAtom "element" |> constr
            let e, nm = processExpr nm e
            let idx = cerl.LInt idx
            modCall erlang el [cerl.Lit idx |> constr; e] |> constr, nm

        let (|ErlangTerm|_|) (t: FSharpType) =
            if t.TypeDefinition.Attributes
               |> Seq.exists (fun a -> a.AttributeType.LogicalName = "ErlangTerm") then
                Some t
            else None

        match expr with
        | B.UnionCaseTest (e, IsFSharpList t, IsCase "Cons" c) ->
            let a1, nm = processExpr nm e
            let a2 = cerl.Lit (cerl.LNil) |> constr
            modCall erlang notEquals [a1; a2] |> constr, nm
        | B.UnionCaseTest (e, IsFSharpList t, IsCase "Empty" c) ->
            let a1, nm = processExpr nm e
            let a2 = cerl.Lit (cerl.LNil) |> constr
            modCall erlang equals [a1; a2] |> constr, nm
        | B.UnionCaseTest (e, IsFSharpOption t, IsCase "Some" c) ->
            let a1, nm = processExpr nm e
            let a2 = litAtom "undefined" |> constr
            modCall erlang notEquals [a1; a2] |> constr, nm
        | B.UnionCaseTest (e, IsFSharpOption t, IsCase "None" c) ->
            let a1, nm = processExpr nm e
            let a2 = litAtom "undefined" |> constr
            modCall erlang equals [a1; a2] |> constr, nm
        | B.UnionCaseTest (e, ErlangTerm t, uc) ->
            let pat, guard = mkErlangTermCasePat nm t uc
            let alt1 = mkAlt pat guard trueExps
            let alt2 = mkAlt (cerl.PVar "_") cerl.defaultGuard falseExps
            let e, ctx = processExpr nm e
            cerl.Case(e, [alt1; alt2]) |> constr, ctx
        | B.UnionCaseTest (e, t, uc) ->
            let pat1 = mkStructuralUnionCasePat t uc
            let alt1 = mkAlt pat1 cerl.defaultGuard trueExps
            let alt2 = mkAlt (cerl.PVar "_") cerl.defaultGuard falseExps
            let e, ctx = processExpr nm e
            cerl.Case(e, [alt1; alt2]) |> constr, ctx
        | B.Call (callee, f, _, argTypes, expressions) ->
            translateCall nm callee f argTypes expressions
        | B.TraitCall (types, name, flags, someTypes, argTypes, args) ->
            let args, nm = foldNames nm processExpr args
            traitCall name args |> constr, nm
            (* modCall fezCore traitCall args |> constr, nm *)
        | B.Value v ->
            let v', nm = safeVar false nm v.LogicalName
            match Map.tryFind v' nm.Functions with
            | Some f ->
                cerl.Fun f |> constr, nm
            | None ->
                let valueExps = cerl.Var v' |> constr
                valueExps, nm
        | B.Const (o, t) ->
            mapConst o t |> constr, nm
        | B.NewTuple (fsType, args) ->
            let args, nm = foldNames nm processExpr args
            let args = List.map (flattenLambda []) args
            cerl.Tuple args |> constr, nm
        | B.TupleGet (fsType, idx, e) ->
            let idx = idx+1
            element nm (int64 idx) e
        | B.NewUnionCase (IsFSharpList t, IsCase "Empty" c, e) ->
            constr (cerl.Lit cerl.LNil), nm
        | B.NewUnionCase (IsFSharpList t, IsCase "Cons" c, e) ->
            let args, nm = foldNames nm processExpr e
            // cons should always generate exactly 2 args
            constr(cerl.List (cerl.LL([args.[0]], args.[1]))), nm
        | B.NewUnionCase (IsFSharpOption t, IsCase "Some" c, [e]) ->
            processExpr nm e
        | B.NewUnionCase (IsFSharpOption t, IsCase "None" c, e) ->
            constr (litAtom "undefined"), nm
        | B.NewUnionCase(ErlangTerm t, uc, []) as e ->
            uc.Name.ToLower() |> litAtom |> constr, nm
        | B.NewUnionCase(ErlangTerm t, uc, [arg]) as e ->
            // a single arg is treated like it's value
            processExpr nm arg
        | B.NewUnionCase(ErlangTerm t, uc, args) as e ->
            // if the DU case has args we treat them as any other tuple
            let args, nm = foldNames nm processExpr args
            let args = List.map (flattenLambda []) args
            cerl.Tuple args |> constr, nm
        | B.NewUnionCase(t, uc, argExprs) as e ->
            let typeTag = mkTypeTag t |> constr
            let caseTag = mkUnionTag uc
            let args, nm = foldNames nm processExpr argExprs
            cerl.Tuple (typeTag :: caseTag:: args) |> constr, nm
        | B.Let ((v, B.Call (_, (LogicalName "receive" as m), _t, [t], _)), expr) as r ->
            // generate basic structural case to for the DU type
            // then generate standard if then else
            let cases = t.TypeDefinition.UnionCases
            let alias, nm = uniqueName nm
            let mkAliasP p = cerl.PAlias (cerl.Alias (alias, p))

            let alts =
                cases
                |> Seq.map (fun c ->
                    let pat = mkStructuralUnionCasePat t c |> mkAliasP
                    cerl.Constr (cerl.Alt (cerl.Pat pat, cerl.defaultGuard,
                                    constr (cerl.Var alias))))
                |> Seq.toList
            let infinity =
                let expiry = litAtom "infinity" |> constr
                let body = litAtom "true" |> constr
                cerl.TimeOut (expiry, body)
            let receive = cerl.Receive (alts, infinity) |> constr
            let n, nm = safeVar true nm v.LogicalName
            let letExps, nm = processExpr nm expr
            mkLet n receive letExps |> constr, nm
        | B.Let ((v, e), expr) ->
            // ignore names introduced in the variable assignment expression
            let ass, _ = processExpr nm e
            let ass = flattenLambda [] ass
            let v', nm = safeVar true nm v.LogicalName
            let next, nm = processExpr nm expr
            mkLet v' ass next |> constr, nm
        | B.IfThenElse (fi, neht, esle) as ite ->
            //plain if then else without decision tree
            let ifExps, nm = processExpr nm fi
            let thenExpr, nm = processExpr nm neht
            let elseExpr, nm = processExpr nm esle
            let a1 = altExpr (boolPat "true", cerl.defaultGuard, thenExpr)
            let a2 = altExpr (boolPat "false", cerl.defaultGuard, elseExpr)
            cerl.Case(ifExps, [a1;a2]) |> constr, nm
        | B.DecisionTree (ite, branches) as tree ->
            let l = List.mapi (fun i x -> i, x) branches |> Map
            let e, nm = processDT nm l ite
            constr e, nm
        | B.FSharpFieldGet (Some e, t, fld) ->
            // TODO when would the expr be None here
            let tupleIndex =
                t.TypeDefinition.FSharpFields
                |> Seq.findIndex ((=) fld)
                |> (+) 1
                |> int64
            let e, nm = processExpr nm e
            tupleGet tupleIndex e |> constr, nm
        | B.NewRecord (t, args) ->
            let args, nm = foldNames nm processExpr args
            //type to atom
            let recordName = mkTypeTag t |> constr
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
        | B.UnionCaseGet (value, IsFSharpOption t, IsCase "Some" c, fld) ->
            processExpr nm value
        | B.UnionCaseGet(e, ErlangTerm t, c, f) ->
            match Seq.toList c.UnionCaseFields with
            | [] -> //atom case
                c.Name.ToLower() |> litAtom |> constr, nm
            | [a] ->
                processExpr nm e
            | args ->
            let idx =
                c.UnionCaseFields
                |> Seq.findIndex ((=) f)
                |> int64
            let element = litAtom "element" |> constr
            let e, nm = processExpr nm e
            let idx = idx + 1L |> cerl.LInt
            modCall erlang element [cerl.Lit idx |> constr; e] |> constr, nm
        | B.UnionCaseGet(e, t, c, f) ->
            // turn these into element/2 calls
            let idx =
                c.UnionCaseFields
                |> Seq.findIndex ((=) f)
                |> int64
            let element = litAtom "element" |> constr
            let e, nm = processExpr nm e
            let idx = idx + 3L |> cerl.LInt
            modCall erlang element [cerl.Lit idx |> constr; e] |> constr, nm
        | B.Coerce(a, e) ->
            processExpr nm e
        | B.NewObject(IsCtor m, types, exprs) ->
            let expss, nm = foldNames nm processExpr exprs
            let expss = expss
                        |> List.choose (function
                                        | cerl.Exp ae -> Some ae
                                        // should be a fold so we can use all Exps
                                        (* | cerl.Exps ae -> Some ae *)
                                        | _ -> None)
            match expss with
            | [e] ->
                cerl.Exp e, nm
            | _ ->
                cerl.Exps (cerl.Constr expss), nm
        // horrendously specific match to intercept printfn and sprintf
        | B.Application (B.Let ((_, B.Call (None, (LogicalName "printfn" | LogicalName "sprintf" as p), _, _,
                                            [B.Coerce (_, B.NewObject (_, _, [B.Const (:? string as str, t)]))])), _letBody),
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
            let arg1 = mapConst str t |> constr
            let args, nm = foldNames nm processExpr args
            let args = [arg1; cerl.List (cerl.L args) |> constr]
            modCall io format args |> constr, nm
        | B.Application (target, _types, args) ->
            let cp = match target with
                     | B.Value f ->
                        let c =
                            f.CurriedParameterGroups
                            |> Seq.length
                        c - (List.length args)
                     | _ -> 0

            let missingArgs, nm =
                foldNames nm (fun nm _ -> uniqueName nm) [1..cp]

            let wrap e =
                if cp > 0 then
                    // wrap in Noop to avoid being flattened later
                    cerl.Noop (lambda missingArgs e |> constr) |> constr
                else e
            let missingArgs =
                missingArgs
                |> List.map (fun a -> cerl.Exp (cerl.Constr (cerl.Var a)))
                |> stripFezUnit
            // if the target is not a plain value or a function we
            // may not be able to process it inline and thus need to wrap it
            // in a Let
            match processExpr nm target with
            | cerl.Exp (cerl.Constr (cerl.Var _ | cerl.Fun _ )) as t, nm ->
                // we're cool the target is just a var or fun - we can inline
                let args, nm = foldNames nm processExpr args
                let args = (args @ missingArgs) |> stripFezUnit
                wrap <| (apply t args |> constr), nm
            | t, nm ->
                let t = flattenLambda [] t
                //the target is something more complex and needs to be
                //wrapped in a Let
                let name, nm = uniqueName nm
                let app, nm =
                    let args, nm = foldNames nm processExpr args
                    apply (varExps name) args |> constr, nm
                mkLet name t app |> constr |> wrap, nm
        | B.Sequential(first, second) ->
            let f, nm = processExpr nm first
            let s, nm = processExpr nm second
            cerl.Seq (f, s) |> constr, nm
        | B.Lambda (IsUnitArg p, expr) ->
            let unitName, nm = safeVar true nm p.LogicalName
            let body, nm = processExpr nm expr
            // wrap body in let so that unit arg is mapped to fez_unit
            let body = mkLet unitName fezUnit body |> constr
            cerl.Lambda ([], body) |> constr, nm
        | B.Lambda (p, expr) ->
            let v, nm = safeVar true nm p.LogicalName
            let body, nm = processExpr nm expr
            let l = cerl.Lambda ([v], body) |> constr
            l, nm
        | B.LetRec(funs, e) ->
            let funDef nm (m : FSharpMemberOrFunctionOrValue, e : FSharpExpr) =
                //TODO do we need to use a safe name?
                let name, nm = safeVar true nm m.LogicalName
                //let recs appear to be unflattened
                //to find numargs we need to process the expr
                //then flatten then take the number of lambda args
                //we have to do it out of order to ensure the function name
                //is processed before the body so processing again after
                let numArgs, l =
                    let e, nm = processExpr nm e
                    let e = e |> flattenLambda []
                    match e with
                    | cerl.Exp (cerl.Constr (cerl.Lambda (args, exps)) as l) ->
                        List.length args, l
                    | args -> failwithf "unexpected letrec args %A" args
                let f, nm = mkFunction nm name numArgs
                let e, nm = processExpr nm e
                let e = e |> flattenLambda []
                funDef f (unconstr e), nm

            let defs, {Functions = fs} = foldNames nm funDef funs
            let nm = {nm with Functions = Map.merge (nm.Functions) fs}
            let e, nm = processExpr nm e
            cerl.LetRec (defs, e) |> constr, nm
        | B.AddressOf e ->
            processExpr nm e
        | B.TryWith(tryExpr, f1, e2, f2, caughtExpr) ->
            let tryExps, nm = processExpr nm tryExpr
            let catchExps = cerl.Catch tryExps |> constr
            let caughtName, nm = safeVar true nm f2.LogicalName
            let p = put (litAtom "last_exception" |> constr) (constr (cerl.Var caughtName))
            let caughtExps, nm = processExpr nm caughtExpr
            let e = cerl.Seq (constr p, caughtExps) |> constr
            mkLet caughtName catchExps e |> constr, nm
            (* failwithf "e1 %A f1 %A e2 %A f2 %A e3 %A" tryExpr f1 e2 f2 caughtExpr *)
        | B.TryFinally(attempt, after) ->
            let att, nm = processExpr nm attempt
            let after, nm = processExpr nm after
            mkTryAfter nm att after
        | B.TypeTest (t, valExpr) when t.TypeDefinition.IsInterface ->
            (*
              FSharp type tuples (records and unions) don't carry information
              about which interfaces they implement only {Module, TypeName}.
              If we just erase typetest we can later attempt to dispatch to the
              the 'Type.IntefaceMethod' using the `trait_call` mechanism.
              *)
            trueExps, nm
        | B.TypeTest (t, valExpr) ->
            //  attempt tuple type test on any other type
            let tag = mkTypeTag t |> constr
            let ele, nm = element nm 1L valExpr
            modCall erlang equals [tag; ele] |> constr, nm
        | B.TypeLambda(_, e) ->
            let e, nm = processExpr nm e
            e, nm
            // why did we need this to be a Noop?
            (* cerl.Noop (lambda [] e |> constr) |> constr, nm *)
        | x -> failwithf "not implemented %A" x

    type ModDecl =
        | Fun of (cerl.Function * cerl.FunDef)
        | Mod of (string * cerl.Module) list
        | Skip

    let rec processModDecl ctx decl =
        let (|HasModCallAttribute|_|) (m :  FSharpMemberOrFunctionOrValue) =
            // TODO check all items in list
            match Seq.toList m.Attributes with
            | [a] when a.AttributeType.FullName = "Fez.Core.ModCall" ->
                Some (Seq.toList a.ConstructorArguments, m)
            | _ -> None

        match decl with
        | MemberOrFunctionOrValue(HasModCallAttribute(args, memb), Parameters ps, _expr) ->
            (* let e, nm = processExpr ctx expr *)
            let e1 = litAtom ((snd args.[0]) :?> string) |> constr
            let e2 = litAtom ((snd args.[1]) :?> string) |> constr
            let args, nm = foldNames ctx (safeVar true) ps
            let e = modCall e1 e2 ((args |> List.map (cerl.Var >> constr))) |> constr
            let l = lambda args e
            //TODO top level functions are unique so no need to prefix
            let name = memb.LogicalName
            let f, nm = mkFunction nm name (List.length ps)
            Fun (f, funDef f l)

        | MemberOrFunctionOrValue(memb, Parameters ps, expr)
            when memb.IsModuleValueOrMember && not memb.IsCompilerGenerated ->
            let atts = Seq.toList memb.Attributes

            let name =
                if memb.EnclosingEntity.FullName = ctx.Module then
                    memb.LogicalName
                else
                    // we're probably a member on a type
                    // make qualified name
                    sprintf "%s.%s" (memb.EnclosingEntity.LogicalName) memb.LogicalName
            let ps =
                match ps with
                | [o; x] when memb.IsMember && memb.IsInstanceMember ->
                    //remove unit on member call
                    [o]
                | _ -> ps
            let args, nm = foldNames ctx (safeVar true) ps
            let e, nm = processExpr ctx expr
            let l = lambda args e
            //TODO top level functions are unique so no need to prefix
            let f, nm = mkFunction nm name (List.length ps)
            Fun (f, funDef f l)
        | Entity(ent, declList) when ent.IsFSharpRecord ->
            Skip
        | Entity(ent, declList) when ent.IsFSharpUnion ->
            Skip
        | Entity(ent, declList) as e when ent.IsFSharpModule ->
            processDecl e |> Mod
        | Entity(ent, declList)  ->
            Skip
        | MemberOrFunctionOrValue(x, _, _) ->
            Skip
        | x -> failwithf "cannot process %A " x


    and processDecl decl = [
      match decl with
      | Entity(ent, implFileDecls) when ent.IsFSharpModule ->
          let name = ent.FullName
          let ctx = Ctx.init name
          let modDecls = List.map (processModDecl ctx) implFileDecls
          let (funs, funDefs) =
              modDecls
              |> List.choose (function
                              | Fun (f, fd) -> Some(f, fd)
                              | _ -> None)
              |> List.unzip
          // add module_info functions
          let mi = [cerl.moduleInfo0 name; cerl.moduleInfo1 name]
          let mif = [cerl.Function (cerl.Atom "module_info", 0)
                     cerl.Function (cerl.Atom "module_info", 1)]
          yield name, cerl.Module (cerl.Atom ent.FullName, funs @ mif, [], funDefs @ mi)
          for md in modDecls do
              match md with
              | Mod decls -> yield! decls
              | _ -> ()
      | InitAction(expr) ->
          failwithf "Module values (InitActions) are not supported as there is no equivalent in erlang.\r\nMake it a function instead.\r\n%A" expr
      | Entity(ent, declList) ->
          failwithf "cannot process record %+A" ent.TryGetMembersFunctionsAndValues
      | x -> failwithf "cannot process %+A" x
    ]

