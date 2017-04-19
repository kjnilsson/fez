module Flat.Compiler

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

    let (|IsField|_|) fieldName (c : FSharpField) =
        if c.Name = fieldName then
            Some fieldName
        else None

    let testModule = """module Test
       let square x = x * x
    """

    let testModule2 = """module Test
       let positive =
           function
           | 0 -> 1
           | n -> n
    """


    let check (checker : FSharpChecker) options (FullPath file) fileContents =
        let res = checker.ParseAndCheckProject options |> run
        if res.HasCriticalErrors then
            failwithf "Errs %A" res.Errors
        res

    let (|Parameters|) (pgs : FSharpMemberOrFunctionOrValue list list) =
        pgs
        |> List.fold (List.append) []
        |> List.map (fun x -> x.FullName, x.FullType.TypeDefinition)

    let altExpr x = cerl.Constr <| cerl.Alt x

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

    let foldNames nm f xs =
        let xs, nm = List.fold (fun (xs, nm) x ->
                                    let x, nm = f nm x
                                    x :: xs, nm) ([], nm) xs
        List.rev xs, nm

    let constr x =
        cerl.Exp (cerl.Constr x)

    let litAtom name =
        cerl.Lit (cerl.LAtom (cerl.Atom name))

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

    let erlang = litAtom "erlang" |> constr

    let (|Intr2Erl|_|) =
        function
        | "op_Multiply" -> Some "*"
        | "op_Addition" -> Some "+"
        | "op_Subtraction" -> Some "-"
        | "op_LessThan" -> Some "<"
        | "op_GreaterThan" -> Some ">"
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

    //TODO: should we consult the FSharpType as well?
    let mapConst (o : obj) =
        match o with
        | :? int as i -> litInt i
        | :? string as s -> litString s
        | :? bool as b -> litAtom (string b)
        | x -> failwithf "mapConst: not impl %A" x

    let (|ExprType|_|) ts (e: FSharpExpr) =
        if e.Type.TypeDefinition.LogicalName = ts then Some e
        else None

    let rec mapCall nm (f : FSharpMemberOrFunctionOrValue) (exprs : FSharpExpr list) : (cerl.Exp * Map<string, int>) =
        match f.LogicalName, exprs with
        | Intr2Erl "+", ExprType "string" _ :: _ ->
            let stringAppend = litAtom "++" |> constr
            let args, nm = foldNames nm processExpr exprs
            modCall erlang stringAppend args, nm
        | Intr2Erl x, _ ->
            let mul = litAtom x |> constr
            let args, nm = foldNames nm processExpr exprs
            modCall erlang mul args, nm
        | name, _ -> //apply
            let func = litAtom name |> constr
            let args, nm = foldNames nm processExpr exprs
            apply func args, nm
        | x, _ ->  failwithf "not implemented %A" x

    // This function is a smell - there must be a better way
    and extractCaseExpr nm e =
        // hacky way to get at the expr between 'match' and 'with'
        match e with
        | B.Call (_, _, _, _, [B.Value v as e;_]) ->
            let e, nm = processExpr nm e
            let v, nm = safeVar false nm v.LogicalName
            v, e, nm
        | B.Let ((v, e), exps) ->
            let e, nm = processExpr nm e
            let v, nm = safeVar false nm v.LogicalName
            v, e, nm
        | B.Call (_, _, _, _, [B.TupleGet (fsType, idx, (B.Value v as e)); _]) ->
            let e, nm = processExpr nm e
            let v, nm = safeVar false nm v.LogicalName
            v, e, nm
        | B.UnionCaseTest (B.Value v as e, t, c) ->
            let e, nm = processExpr nm e
            let v, nm = safeVar false nm v.LogicalName
            v, e, nm
        | x -> failwithf "caseExpr not imp %A" x

    and processPat nm (expr : FSharpExpr) : (cerl.Pat * cerl.Guard * Map<string, int>) =
        match expr with
        // special case for matching against literal values
        | B.Call (_expr, f, _, _fsType, [B.Value (_); B.Const (Lit cVal, _)])
            when f.LogicalName = "op_Equality" ->
            cerl.PLit cVal, cerl.Guard (litAtom "true" |> constr), nm
        | B.Call (_expr, mfv, _, _fsType, [B.TupleGet (tt, idx, v)
                                           B.Const (Lit cVal, _)])
            when mfv.LogicalName = "op_Equality" ->
            // create inline tuple match
            let litPat = cerl.PLit cVal
            let tupleSize = tt.GenericArguments |> Seq.length
            let tpat = [for i in 0..tupleSize-1 do
                            if i = idx then yield litPat
                            else yield cerl.PVar "_" ]
            cerl.PTuple tpat, cerl.Guard (litAtom "true" |> constr), nm
        | B.Call (_expr, mfv, _, _fsType, [left; right])
            when mfv.LogicalName = "op_Equality" ->

            (* printfn "bah %A %A" _expr expr *)
            let leftExps, nm = processExpr nm left
            let rightExps, nm = processExpr nm right
            let eq = litAtom "=:=" |> constr
            let guardExps = modCall erlang eq [leftExps; rightExps] |> constr
            cerl.PVar "_", cerl.Guard guardExps, nm
            (* cerl.PList (cerl.LL ([cerl.PLit cVal], cerl.PVar "_")), cerl.defaultGuard, nm *)
                //list pattern constant
        | B.Let ((v, B.UnionCaseGet(_, IsFSharpList t, _, IsField "Tail" fld)), expr) ->
            (* printfn "processPat let tail expr %A" expr *)
            let v, nm = safeVar true nm v.LogicalName
            let thisPat = cerl.PList (cerl.LL ([], cerl.PVar v))
            // recurse over let binding patterns
            // maybe all this should be guards
            let guard, nm = processExpr nm expr
            let pat = thisPat
            pat, cerl.Guard guard, nm
            (* let nextPat, guard, nm = processPat nm expr *)
            (* let pat = cerl.mergePat (thisPat,  nextPat) *)
            (* pat, guard, nm *)
        | B.Let ((v, B.UnionCaseGet(_, IsFSharpList t, _, IsField "Head" fld)), expr) ->
            let v, nm = safeVar true nm v.LogicalName
            (* printfn "processPat let head this %A gurad %A expr %A" thisPat guard expr *)
            let thisPat = cerl.PList(cerl.LL ([cerl.PVar v], cerl.PVar "_"))
            let guard, nm = processExpr nm expr
            let pat = thisPat
            pat, cerl.Guard guard, nm
        | B.Let ((v, caseExps), expr) ->
            let v, nm = safeVar true nm v.LogicalName
            let guardExps, nm = processExpr nm expr
            cerl.PVar v, cerl.Guard guardExps, nm
        | B.Call (_expr, f, _, _typeSig, [B.Value v; B.Const (Lit cVal, _)]) as expr ->
            // try to create a guard from call
            let v, nm = safeVar true nm v.LogicalName
            let guardExps, nm = processExpr nm expr
            cerl.PVar v, cerl.Guard guardExps, nm
        | B.UnionCaseTest (e, IsFSharpList t, IsCase "Cons" c) ->
            //Cons cell without any name bindings
            cerl.PList (cerl.LL ([cerl.PVar "_"], cerl.PVar "_")), cerl.defaultGuard, nm
            (* failwithf "processPat: %+A %+A %+A" e c.CompiledName c.Name *)

        | expr ->
            failwithf "processPat not implemented %A" expr

    and processITEs d nm expr : (FSharpExpr * (cerl.Pat * cerl.Guard * Map<string, int>)) list =
        [
        match expr with
        | B.IfThenElse(fi, (B.DecisionTreeSuccess(_, _) as e), esle) ->
            let pat, guard, nm' = processPat nm fi
            yield e, (pat, guard, nm')
            // need to pass orig nm here as each branch need the same outer scope
            yield! processITEs d nm esle
        | B.IfThenElse(fi, neht, esle) ->
            let pat, guard, nm' = processPat nm fi
            // need to pass orig nm here as each branch need the same outer scope
            let thenPats = processITEs d nm neht
            let nextExpr = match thenPats with
                           | (nextExpr, _) :: _ -> nextExpr
                           | _ -> failwith "could not get index"
            let thenPatsGrouped =
                (nextExpr, (pat, guard, nm')) :: thenPats
                |> List.groupBy fst
                |> List.map (fun (k, v) -> (k, List.map snd v))

            (* printfn " thenPatsGrouped%A"  (thenPatsGrouped) *)
            // TODO:
            let wrap a (cerl.Guard b) =
                let alt1 = altExpr (cerl.Pats [], a, b)
                let alt2 = altExpr (cerl.Pats [], cerl.defaultGuard, litAtom "false" |> constr)
                cerl.Guard (constr <| cerl.Case (cerl.Exps (cerl.Constr []), [alt1;alt2]))

            let mergeGuards (a, b) =
                            if a = cerl.defaultGuard then b
                            elif b = cerl.defaultGuard then a
                            else wrap a b

            let merged = thenPatsGrouped
                         |> List.map (fun (k, vs) ->
                                let s = List.head vs
                                let rest = List.tail vs
                                k, List.fold (fun (ps, gs, ctxs)
                                               (p, g, ctx) ->
                                                    cerl.mergePat (ps, p),
                                                        mergeGuards (gs, g),
                                                            ctx)
                                           s rest)

            yield! merged
            yield! processITEs d nm esle
        (* | B.DecisionTreeSuccess(idx, []) as e -> *)
        | e ->
            // TODO PVar - need to pass it
            yield e, (cerl.PVar "_", cerl.Guard (litAtom "true" |> constr), nm)
        | x -> failwithf "processITE not impl %A" x ]


    and processExpr nm (expr : FSharpExpr) : (cerl.Exps * Map<string, int>) =
        let res, nmOut =
            match expr with
            | B.Call (_expr, f, _, _typeSig, expressions) ->
                mapCall nm f expressions
            | B.Value v ->
                let v', nm = safeVar false nm v.LogicalName
                cerl.Var v', nm
            | B.Const (o, t) ->
                mapConst o, nm
            | B.NewTuple (fsType, args) ->
                let args, nm = foldNames nm processExpr args
                cerl.Tuple args, nm
            | B.TupleGet (fsType, idx, e) ->
                let element = litAtom "element" |> constr
                let e, nm = processExpr nm e
                let idx = idx+1 |> cerl.LInt
                modCall erlang element [cerl.Lit idx |> constr; e], nm
            | B.NewUnionCase(fsType, fsUnionCase, argExprs) as e ->
                let unionTag = fsUnionCase.CompiledName |> litAtom |> constr
                let args, nm = foldNames nm processExpr argExprs
                cerl.Tuple (unionTag :: args), nm
                (* failwithf "NewUnionCase not impl %A" e *)
            | B.Let ((v, e), expr) as l ->
                let ass, nm = processExpr nm e
                let v', nm = safeVar true nm v.LogicalName
                let next, nm = processExpr nm expr
                mkLet v' ass next, nm
            | B.IfThenElse (fi, neht, esle) as ite ->
                //plain if then else without decision tree
                (* printfn "if %A\r\nthen %A\r\nelse %A" fi neht esle *)
                let caseExprVar, caseExpr, nm = extractCaseExpr nm fi
                match processITEs caseExprVar nm ite with
                | [thenExpr, (ifPat, ifGuard, _)
                   elseExpr, (elsePat, elseGuard, _)]  ->
                        let thenExps, _ = processExpr nm thenExpr
                        let elseExps, _ = processExpr nm elseExpr
                        let alts = [altExpr (cerl.Pat ifPat, ifGuard, thenExps)
                                    altExpr (cerl.Pat elsePat, elseGuard, elseExps)]

                        cerl.Case (caseExpr, alts), nm
                | x -> failwithf "unexpected if then else result %A" x

            | B.DecisionTree (B.IfThenElse (fi, _, _) as ite, l) as tree ->
                (* let (lvals, l2) = List.head l *)
                (* printfn "ite %A l %A" ite  l *)
                // TODO: it wont always be vars
                let caseExprVar, caseExpr, nm = extractCaseExpr nm fi
                let ites = processITEs caseExprVar nm ite
                           |> List.map (fun (k, v) ->
                                            match k with
                                            | B.DecisionTreeSuccess(idx, targetValueExprs) ->
                                                idx, (targetValueExprs, v)
                                            | x -> failwithf "unexpected %A" x)
                           |> Map

                let alts : List<cerl.Ann<cerl.Alt>> =
                    l
                    |> List.mapi (fun i (mfvs, e) ->
                            //merge mfvs with valueExprs + pat
                            let valueExprs, (pat, grd, nm) = ites.[i]
                            let mfvs = mfvs |> List.map (fun v -> v.CompiledName)
                            let assignments, mn =
                                List.zip mfvs valueExprs
                                |> List.fold (fun (agg, nm) (v, e) ->
                                    let v, nm = safeVar false nm v
                                    let e, nm = processExpr nm e
                                    ((v, e) :: agg), nm) ([], nm)

                            let e, _ = processExpr nm e
                            (* printfn "merged pat %A" pat *)
                            let vls = List.map fst assignments
                            let es = List.choose (fun (_, e) ->
                                            match e with
                                            | cerl.Exp ae -> Some ae
                                            | _ -> None) assignments
                            let e =
                                cerl.Let ((vls, cerl.Exps (cerl.Constr es)), e)
                            // we can throw away nm now as have branched

                            // TODO if e is a let it should be in pattern
                            altExpr (cerl.Pat pat, grd, e |> constr))
                cerl.Case (caseExpr, alts), nm
            | B.FSharpFieldGet (Some e, t, fld) ->
                // TODO when would the expr be None here
                let tupleIndex =
                    t.TypeDefinition.FSharpFields
                    |> Seq.findIndex ((=) fld)
                    |> (+) 1
                let e, nm = processExpr nm e
                tupleGet tupleIndex e, nm
            | B.NewRecord (t, args) ->
                let args, nm = foldNames nm processExpr args
                //type to atom
                let recordName =
                    litAtom t.TypeDefinition.LogicalName |> constr
                cerl.Tuple (recordName :: args), nm
            | B.UnionCaseGet(value, IsFSharpList fsType, IsCase "Cons" uCas,
                             IsField "Head" fld) ->

                 let hd = litAtom "hd" |> constr
                 let e, nm = processExpr nm value
                 modCall erlang hd [e], nm
            | B.UnionCaseGet(value, IsFSharpList fsType, IsCase "Cons" uCas,
                             IsField "Tail" fld) ->
                 let hd = litAtom "tl" |> constr
                 let e, nm = processExpr nm value
                 modCall erlang hd [e], nm
            | x -> failwithf "not implemented %A" x
        constr res, nmOut


    let processModDecl decl =
      match decl with
      | MemberOrFunctionOrValue(memb, Parameters ps, expr)
          when memb.IsModuleValueOrMember && not memb.IsCompilerGenerated ->
              (* printfn "memb %A" memb.LogicalName *)
              (* failwithf "expr %A" expr *)
              let nm = Map.empty
              let args, nm = foldNames nm (safeVar true) (List.map fst ps)
              let e, nm = processExpr nm expr
              let l = lambda args e
              //TODO make function name safe
              let f = mkFunction memb.LogicalName (List.length ps)
              Some (f, funDef f l)
      | Entity(ent, declList) when ent.IsFSharpRecord ->
          (* printfn "cannot process record %+A" (ent.FSharpFields |> Seq.toList) *)
          None
      |  MemberOrFunctionOrValue(x, _, _) ->
          (* printfn "cannot process %A" x.LogicalName *)
          None
      | x -> failwithf "cannot process %A" x


    let processDecl decl =
      match decl with
      | Entity(ent, implFileDecls) when ent.IsFSharpModule ->
          let (funs, funDefs) =
              implFileDecls
              |> List.choose processModDecl
              |> List.unzip
          cerl.Module (cerl.Atom ent.LogicalName, funs, [], funDefs)
      | InitAction(expr) ->
          failwithf "Init Action not supported %A" expr
      | Entity(ent, declList) ->
          failwithf "cannot process record %+A" ent.TryGetMembersFunctionsAndValues
      | x -> failwithf "cannot process %+A" x

