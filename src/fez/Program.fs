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
    try
        let sysCoreLib = typeof<System.Object>.GetTypeInfo().Assembly.Location
        let sysPath = Path.GetDirectoryName(sysCoreLib)
        let files = argv |> Array.map (|FullPath|)
        let checker = FSharpChecker.Create(keepAssemblyContents = true)
        let options = projectOptions checker files
        let outFiles = new System.Collections.Generic.List<_>()
        for file in files do
            let fileContents = File.ReadAllText file
            let dir = Path.GetDirectoryName file
            let res = check checker options file fileContents
            let decs = res.AssemblyContents.ImplementationFiles.Head.Declarations
            for implFile in res.AssemblyContents.ImplementationFiles do
              for decl in implFile.Declarations do
                  (* failwithf "%A" decl *)
                  let modules = processDecl decl
                  for n, m in modules do
                      (* printfn "final ast: %A" m *)
                      cerl.prt m |> writeCoreFile dir n
                      let fileInfo = FileInfo(Path.Combine(dir, n + ".core"))
                      outFiles.Add(fileInfo.FullName)
                      
        for file in outFiles do
            printfn "%s" file
        0
    with
    | e ->
        Environment.Exit 1
        reraise()
        1