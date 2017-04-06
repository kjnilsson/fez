#!/usr/bin/env fsharpi
#r "./lib/FSharp.Compiler.Service.dll"
#load "./flatc/cerl.fs"
#load "./flatc/Program.fs"
open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices

let testModule = """module Test
   let square x = x * x
"""

let testModule2 = """module Test
   let positive =
       function
       | 0 -> 1
       | n -> n
"""

let check testModule =
    let file = __SOURCE_DIRECTORY__ +  "/Test.fs"
    File.WriteAllText(file, testModule)
    let checker = FSharpChecker.Create(keepAssemblyContents=true)
    let options =
      checker.GetProjectOptionsFromCommandLineArgs("Test", [|"-o:Test.dll";"-a";file|])
    let res =
      checker.ParseAndCheckProject(options) |> Async.RunSynchronously
    assert(Array.empty = res.Errors)
    res

let res = check testModule
let decs = res.AssemblyContents.ImplementationFiles.Head.Declarations

let dec = decs.[0]
let Entity(ent, decls) = dec

let (ent, decls) =
    match dec with
    | Entity(ent, decls) ->  (ent, decls)
    | _ -> failwith "noo"

let (|Parameters|) (pgs : FSharpMemberOrFunctionOrValue list list) =
    pgs
    |> List.fold (List.append) []
    |> List.map (fun x -> x.FullName, x.FullType.TypeDefinition)

let (MemberOrFunctionOrValue(m, Parameters arg, expr)) = decls.[0]
expr
(* m.FullType.GenericArguments.[0] *)

let constr x =
    cerl.Exp (cerl.Constr x)

let litAtom name =
    cerl.Lit (cerl.LAtom (cerl.Atom name))

let modCall left right exps =
    cerl.ModCall ((left, right), exps)

let lambda args expr =
    cerl.Lambda (args, expr)

let mkFunction name arity =
    cerl.Function (cerl.Atom name, arity)

let funDef f (expr) =
    cerl.FunDef (cerl.Constr f, cerl.Constr expr)

let rec mapFunction (f : FSharpMemberOrFunctionOrValue) exprs : cerl.Exp =
    match f.LogicalName, exprs with
    | "op_Multiply", [left; right] ->
        let erlang = litAtom "erlang" |> constr
        let mul = litAtom "*" |> constr
        modCall erlang mul (List.map processExpr exprs)
    | x ->  failwithf "not implemented %A" x

and processExpr expr : cerl.Exps =
    match expr with
    | BasicPatterns.Call (_expr, f, _, _typeSig, expressions) ->
        mapFunction f expressions |> constr
    | BasicPatterns.Value v ->
        cerl.Var v.DisplayName |> constr
    | x -> failwithf "not implemented %A" x

processExpr expr

let processModDecl decl =
  match decl with
  | MemberOrFunctionOrValue(memb, Parameters ps, expr)
      when memb.IsModuleValueOrMember ->
          let e = processExpr expr
          let args = ps |> List.map (fun (x, _) -> x)
          let l = lambda args e
          let f = mkFunction memb.LogicalName (List.length ps)
          f, funDef f l
  | x -> failwithf "cannot process %A" x

processModDecl decls.[0]

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

for implFile in res.AssemblyContents.ImplementationFiles do
  for decl in implFile.Declarations do
      let m = processDecl decl
      cerl.prt m

