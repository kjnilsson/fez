module Fez.Program

open System
open System.Reflection
open System.IO
open Fez.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.SourceCodeServices.BasicPatterns


let writeCoreFile dir name text =
    let path = Path.Combine(dir, name + ".core")
    File.WriteAllText(path, text)

[<EntryPoint>]
let main argv =
    let sysCoreLib = typeof<System.Object>.GetTypeInfo().Assembly.Location
    let sysPath = Path.GetDirectoryName(sysCoreLib)
    match argv with
    | [|FullPath file|] ->
        let fileContents = File.ReadAllText file
        let dir = Path.GetDirectoryName file
        let checker = FSharpChecker.Create(keepAssemblyContents = true)
        let options = projectOptions checker file
        let res = check checker options file fileContents
        let decs = res.AssemblyContents.ImplementationFiles.Head.Declarations
        for implFile in res.AssemblyContents.ImplementationFiles do
          for decl in implFile.Declarations do
              (* failwithf "%A" decl *)
              let modules = processDecl decl
              (* printfn "final ast: %A" m *)
              for n, m in modules do
                  cerl.prt m |> writeCoreFile dir n
        0
    | _ ->
        failwithf "Uknnown args %A" argv
