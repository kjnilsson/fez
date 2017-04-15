module Flat

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
    let compilerArgs =
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
             ++ compilerArgs
         ReferencedProjects = [||]
         IsIncompleteTypeCheckEnvironment = false
         UseScriptResolutionRules = false
         LoadTime = DateTime.Now
         UnresolvedReferences = None
         OriginalLoadReferences = []
         ExtraProjectInfo = None}


[<AutoOpen>]
module Compiler =

    let testModule = """module Test
       let square x = x * x
    """

    let testModule2 = """module Test
       let positive =
           function
           | 0 -> 1
           | n -> n
    """

    let check (FullPath file) fileContents =
        let checker = FSharpChecker.Create(keepAssemblyContents = true)
        let options = projectOptions checker file
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
        let erlang = litAtom "erlang" |> constr
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
            let erlang = litAtom "erlang" |> constr
            let stringAppend = litAtom "++" |> constr
            let args, nm = foldNames nm processExpr exprs
            modCall erlang stringAppend args, nm
        | Intr2Erl x, _ ->
            let erlang = litAtom "erlang" |> constr
            let mul = litAtom x |> constr
            let args, nm = foldNames nm processExpr exprs
            modCall erlang mul args, nm
        | name, _ -> //apply
            let func = litAtom name |> constr
            let args, nm = foldNames nm processExpr exprs
            apply func args, nm
        | x, _ ->  failwithf "not implemented %A" x

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
        | x -> failwithf "caseExpr not imp %A" x

    and processPat nm (expr : FSharpExpr) : (cerl.Pat * cerl.Guard * Map<string, int>) =
        match expr with
        // special case for matching against literal values
        | B.Call (_expr, f, _, _typeSig, [B.Value (_); B.Const (Lit cVal, _)])
            when f.LogicalName = "op_Equality" ->
            cerl.PLit cVal, cerl.Guard (litAtom "true" |> constr), nm
        | B.Call (_expr, mfv, _, _typeSig, [B.TupleGet (tt, idx, v)
                                            B.Const (Lit cVal, _)])
            when mfv.LogicalName = "op_Equality" ->
            // create inline tuple match
            let litPat = cerl.PLit cVal
            let tupleSize = tt.GenericArguments |> Seq.length
            let tpat = [for i in 0..tupleSize-1 do
                            if i = idx then yield litPat
                            else yield cerl.PVar "_" ]
            cerl.PTuple tpat, cerl.Guard (litAtom "true" |> constr), nm
        | B.Let ((v, _caseExps), expr) ->
            let v, nm = safeVar true nm v.LogicalName
            let guardExps, nm = processExpr nm expr
            cerl.PVar v, cerl.Guard guardExps, nm
        | B.Call (_expr, f, _, _typeSig, [B.Value v; B.Const (Lit cVal, _)]) as expr ->
            // try to create a guard from call
            let v, nm = safeVar true nm v.LogicalName
            let guardExps, nm = processExpr nm expr
            cerl.PVar v, cerl.Guard guardExps, nm

        | expr ->
            failwithf "not implemented %A" expr

    and processITEs d nm expr : (int * (cerl.Pat * cerl.Guard * Map<string, int>)) list =
        [
        match expr with
        | B.IfThenElse(fi, B.DecisionTreeSuccess(idx, []), esle) ->
            let pat, guard, nm' = processPat nm fi
            yield idx, (pat, guard, nm')
            // need to pass orig nm here as each branch need the same outer scope
            yield! processITEs d nm esle
        | B.IfThenElse(fi, neht, esle) ->
            let pat, guard, nm' = processPat nm fi
            // need to pass orig nm here as each branch need the same outer scope
            let thenPats = processITEs d nm neht
            let idx = match thenPats with
                      | (idx, _) :: _ -> idx
                      | _ -> failwith "could not get index"
            let thenPatsGrouped =
                (idx, (pat, guard, nm')) :: thenPats
                |> List.groupBy fst |> Map
                |> Map.map (fun k v -> List.map snd v)

            let mergePat (a, b) =
                match (a,b) with
                | cerl.PTuple aps, cerl.PTuple bps ->
                    List.zip aps bps
                    |> List.map (function
                                 | cerl.PVar "_", b -> b
                                 | a, cerl.PVar "_" -> a
                                 | x -> failwithf "dont know how to merge %A" x)
                    |> cerl.PTuple
                | x -> failwithf "mergePat not impl %A" x

            // TODO:
            let mergeGuards (a, b) = a
            let merged = thenPatsGrouped
                         |> Map.map (fun _ vs ->
                                let s = List.head vs
                                let rest = List.tail vs
                                List.fold (fun (ps, gs, ctxs)
                                               (p, g, ctx) ->
                                                    mergePat (ps, p),
                                                        mergeGuards (gs, g),
                                                            ctx)
                                           s rest)

            (* printfn "merged %A"  (thenPatsGrouped, merged) *)
            yield! merged |> Map.toList
            yield! processITEs d nm esle
        | B.DecisionTreeSuccess(idx, []) ->
            // TODO PVar - need to pass it
            yield idx, (cerl.PVar d, cerl.Guard (litAtom "true" |> constr), nm)
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
                let erlang = litAtom "erlang" |> constr
                let element = litAtom "element" |> constr
                let e, nm = processExpr nm e
                let idx = idx+1 |> cerl.LInt
                modCall erlang element [cerl.Lit idx |> constr; e], nm
            | B.Let ((v, e), expr) as l ->
                let ass, nm = processExpr nm e
                let v', nm = safeVar true nm v.LogicalName
                let next, nm = processExpr nm expr
                mkLet v' ass next, nm
            | B.IfThenElse (fi, neht, esle) ->
                //plain if then else without decision tree
                let caseExprVar, caseExpr, nm = extractCaseExpr nm fi
                let pat, guard, nm' = processPat nm fi
                let thenExps, _ = processExpr nm' neht
                let elseExps, _ = processExpr nm esle
                let defPat = cerl.PVar caseExprVar
                let defGuard = cerl.defaultGuard
                let alts = [altExpr (cerl.Pat pat, guard, thenExps)
                            altExpr (cerl.Pat defPat, defGuard, elseExps)]

                cerl.Case (caseExpr, alts), nm
            | B.DecisionTree (B.IfThenElse (fi, _, _) as ite, l) as tree ->
                (* printfn "tree %A" tree *)
                // TODO: it wont always be vars
                let caseExprVar, caseExpr, nm = extractCaseExpr nm fi
                let ites = processITEs caseExprVar nm ite |> Map
                let alts : List<cerl.Ann<cerl.Alt>> =
                    l
                    |> List.mapi (fun i (mfvs, e) ->
                            let pat, grd, nm = ites.[i]
                            // we can throw away nm now as have branched
                            let e, _ = processExpr nm e
                            // TODO if e is a let it should be in pattern
                            altExpr (cerl.Pat pat, grd, e))
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
            | x -> failwithf "not implemented %A" x
        constr res, nmOut


    let processModDecl decl =
      match decl with
      | MemberOrFunctionOrValue(memb, Parameters ps, expr)
          when memb.IsModuleValueOrMember && not memb.IsCompilerGenerated ->
              printfn "memb %A" memb.LogicalName
              (* failwithf "expr %A" expr *)
              let nm = Map.empty
              let args, nm = foldNames nm (safeVar true) (List.map fst ps)
              let e, nm = processExpr nm expr
              let l = lambda args e
              //TODO make function name safe
              let f = mkFunction memb.LogicalName (List.length ps)
              Some (f, funDef f l)
      | Entity(ent, declList) when ent.IsFSharpRecord ->
          printfn "cannot process record %+A" (ent.FSharpFields |> Seq.toList)
          None
      |  MemberOrFunctionOrValue(x, _, _) ->
          printfn "cannot process %A" x.LogicalName
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

[<EntryPoint>]
let main argv =
    let sysCoreLib = typeof<System.Object>.GetTypeInfo().Assembly.Location
    let sysPath = Path.GetDirectoryName(sysCoreLib)
    match argv with
    | [|FullPath file|] ->
        let fileContents = File.ReadAllText file
        let res = check file fileContents
        let decs = res.AssemblyContents.ImplementationFiles.Head.Declarations
        for implFile in res.AssemblyContents.ImplementationFiles do
          for decl in implFile.Declarations do
              (* failwithf "%A" decl *)
              let m = processDecl decl
              cerl.prt m |> printfn "%s"
        0
    | _ ->
        failwithf "Uknnown args %A" argv
