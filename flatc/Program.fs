module Flat

open System
open System.Reflection
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices

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
        | _ -> None

    let (|Lit|_|) (o: obj) =
        match o with
        | :? int as i -> cerl.LInt i |> Some
        | x -> None


    //TODO: should we consult the FSharpType as well?
    let mapConst (o : obj) =
        match o with
        | :? int as i -> litInt i
        | x -> failwithf "mapConst: not impl %A" x

    let rec mapCall nm (f : FSharpMemberOrFunctionOrValue) (exprs : FSharpExpr list) : (cerl.Exp * Map<string, int>) =
        match f.LogicalName with
        | Intr2Erl x ->
            let erlang = litAtom "erlang" |> constr
            let mul = litAtom x |> constr
            let args, nm = foldNames nm processExpr exprs
            modCall erlang mul args, nm
        | name -> //apply
            let func = litAtom name |> constr
            let args, nm = foldNames nm processExpr exprs
            apply func args, nm
        | x ->  failwithf "not implemented %A" x

    and processPat nm (expr : FSharpExpr) : (cerl.Pat * cerl.Guard * Map<string, int>) =
        match expr with
        | BasicPatterns.Call (_expr, f, _, _typeSig, [BasicPatterns.Value (_)
                                                      BasicPatterns.Const (Lit cVal, _)])
            when f.LogicalName = "op_Equality" ->
            cerl.PLit cVal, cerl.Guard (litAtom "true" |> constr), nm
            (* mapCall nm f expressions *)
        (* | BasicPatterns.Const (o, t) -> *)
        (*     mapConst o, nm *)
        | x -> failwithf "not implemented %A" x

    and processITEs d nm expr : (int * (cerl.Pat * cerl.Guard * Map<string, int>)) list =
        [
        match expr with
        | BasicPatterns.IfThenElse(fi, BasicPatterns.DecisionTreeSuccess(idx, []), esle) ->
            let pat, guard, nm' = processPat nm fi
            yield idx, (pat, guard, nm')
            // need to pass orig nm here as each branch need the same outer scope
            yield! processITEs d nm esle
        | BasicPatterns.DecisionTreeSuccess(idx, []) ->
            // TODO PVar - need to pass it
            yield idx, (cerl.PVar d, cerl.Guard (litAtom "true" |> constr), nm)
        | x -> failwithf "processITE not impl %A" x ]


    and processExpr nm (expr : FSharpExpr) : (cerl.Exps * Map<string, int>) =
        let res, nmOut =
            match expr with
            | BasicPatterns.Call (_expr, f, _, _typeSig, expressions) ->
                mapCall nm f expressions
            | BasicPatterns.Value v ->
                let v', nm = safeVar false nm v.LogicalName
                cerl.Var v', nm
            | BasicPatterns.Const (o, t) ->
                mapConst o, nm
            | BasicPatterns.Let ((v, e), expr) as l ->
                let ass, nm = processExpr nm e
                let v', nm = safeVar true nm v.LogicalName
                let next, nm = processExpr nm expr
                mkLet v' ass next, nm
            | BasicPatterns.DecisionTree (BasicPatterns.IfThenElse (iff, thenn, elsee) as e, l) as tree ->
                printfn "tree %A" tree
                // hacky way to get at the expr between 'match' and 'with'
                let v, caseExpr, nm =
                    match iff with
                    | BasicPatterns.Call (_, _, _, _, [e1;_]) ->
                        match processExpr nm e1 with
                        | cerl.Exp (cerl.Constr (cerl.Var v)) as e, nm -> v, e, nm
                        | x -> failwithf "caseExprinner not imp %A" x
                    | x -> failwithf "caseExpr not imp %A" x

                let ites = processITEs v nm e |> Map
                let alts : List<cerl.Ann<cerl.Alt>> =
                    l |> List.mapi (fun i (mfvs, e) ->
                    let pat, grd, nm = ites.[i]
                    // we can throw away nm now as have branched
                    let e, _ = processExpr nm e
                    cerl.Constr <| cerl.Alt (cerl.Pat pat, grd, e))
                cerl.Case (caseExpr, alts), nm
            | x -> failwithf "not implemented %A" x
        constr res, nmOut


    let processModDecl decl =
      match decl with
      | MemberOrFunctionOrValue(memb, Parameters ps, expr)
          when memb.IsModuleValueOrMember ->
              let nm = Map.empty
              let args, nm = foldNames nm (safeVar true) (List.map fst ps)
              let e, nm = processExpr nm expr
              let l = lambda args e
              //TODO make function name safe
              let f = mkFunction memb.LogicalName (List.length ps)
              f, funDef f l
      | x -> failwithf "cannot process %A" x


    let processDecl decl =
      match decl with
      | Entity(ent, declList) when ent.IsFSharpModule ->
          let (funs, funDefs) =
              declList
              |> List.map processModDecl
              |> List.unzip
          cerl.Module (cerl.Atom ent.LogicalName, funs, [], funDefs)
      | InitAction(expr) ->
          failwithf "Init Action not supported %A" expr
      | x -> failwithf "cannot process %A" x

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
              let m = processDecl decl
              cerl.prt m |> printfn "%s"
        0
    | _ ->
        failwithf "Uknnown args %A" argv
