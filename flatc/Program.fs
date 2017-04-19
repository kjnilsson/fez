module Flat.Program

open System
open System.Reflection
open System.IO
open Flat.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.SourceCodeServices.BasicPatterns
[<EntryPoint>]
let main argv =
    let sysCoreLib = typeof<System.Object>.GetTypeInfo().Assembly.Location
    let sysPath = Path.GetDirectoryName(sysCoreLib)
    match argv with
    | [|FullPath file|] ->
        let fileContents = File.ReadAllText file
        let checker = FSharpChecker.Create(keepAssemblyContents = true)
        let options = projectOptions checker file
        let res = check checker options file fileContents
        let decs = res.AssemblyContents.ImplementationFiles.Head.Declarations
        for implFile in res.AssemblyContents.ImplementationFiles do
          for decl in implFile.Declarations do
              (* failwithf "%A" decl *)
              let m = processDecl decl
              (* printfn "final ast: %A" m *)
              cerl.prt m |> printfn "%s"
        0
    | _ ->
        failwithf "Uknnown args %A" argv
