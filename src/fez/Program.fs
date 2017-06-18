module Fez.Program

open System
open System.Reflection
open System.IO
open System.Diagnostics
open Fez.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.SourceCodeServices.BasicPatterns



[<EntryPoint>]
let main argv =
    try
        let outputPath,argv =
            match argv |> Array.tryFindIndex (fun p -> p = "-o" || p = "--output") with
            | Some i when i < argv.Length - 1 ->
                let di = DirectoryInfo argv.[i+1]
                let argv =
                    Array.indexed argv
                    |> Array.filter (fun (p,a) -> p <> i && p <> (i + 1))
                    |> Array.map snd
                if not di.Exists then
                    di.Create()
                Some di.FullName,argv
            | _ -> None,argv

        let noBeam,argv =
            match argv |> Array.tryFindIndex ((=) "--nobeam") with
            | Some i ->
                true,argv |> Array.filter ((<>) "--nobeam")
            | _ -> false,argv
        
        let outputPath = ref outputPath
        let sysCoreLib = typeof<System.Object>.GetTypeInfo().Assembly.Location
        let sysPath = Path.GetDirectoryName(sysCoreLib)
        let files = argv |> Array.map (|FullPath|)
        let checker = FSharpChecker.Create(keepAssemblyContents = true)
        let options = projectOptions checker files
        let outFiles =
            [ for file in files do
                let fileContents = File.ReadAllText file
                let dir = 
                    match !outputPath with
                    | Some path -> path
                    | _ ->
                        let dir = Path.GetDirectoryName file
                        outputPath := Some dir
                        dir

                let res = check checker options file fileContents
                let decs = res.AssemblyContents.ImplementationFiles.Head.Declarations
                for implFile in res.AssemblyContents.ImplementationFiles do
                  for decl in implFile.Declarations do
                      (* failwithf "%A" decl *)
                      let modules = processDecl decl
                      for n, m in modules do
                          (* printfn "final ast: %A" m *)
                          yield
                              cerl.prt m 
                              |> ErlangCompilerInterop.writeErlangCoreFile dir n ]

        if not noBeam then
            ErlangCompilerInterop.erlc !outputPath outFiles            
        0
    with
    | exn ->
        printfn "fez failed with:" // TODO: write in red to stderr
        printfn "%s" exn.Message
        Environment.Exit 1
        1