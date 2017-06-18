module Fez.Program

open System
open System.Reflection
open System.IO
open System.Diagnostics
open Fez.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.SourceCodeServices.BasicPatterns

let erlc output files =
    if Seq.isEmpty files then () else
    let out = new System.Collections.Generic.List<_>()
    let errors = new System.Collections.Generic.List<_>()
    use proc = new Process()
    proc.StartInfo.UseShellExecute <- false
    proc.StartInfo.FileName <- "erlc"
    proc.StartInfo.Arguments <- 
        let mutable args = sprintf "-v -o \"%s\"" output 
        for file in files do
            args <- args + sprintf " \"%s\"" file
        args
    proc.StartInfo.WorkingDirectory <- output
    proc.StartInfo.RedirectStandardOutput <- true
    proc.StartInfo.RedirectStandardError <- true
    
    proc.ErrorDataReceived.Add(fun d -> if not (isNull d.Data) then errors.Add d.Data)
    proc.OutputDataReceived.Add(fun d -> if not (isNull d.Data) then out.Add d.Data)
    proc.Start() |> ignore
    proc.BeginErrorReadLine()
    proc.BeginOutputReadLine()
    proc.WaitForExit()
    if proc.ExitCode <> 0 then
        let lines = String.Concat(errors |> Seq.map (fun e -> Environment.NewLine + e))
        failwithf "erlc failed: %s" lines


let writeCoreFile dir name text =
    let path = FileInfo(Path.Combine(dir, name + ".core")).FullName
    File.WriteAllText(path, text)
    path

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
        let outFiles = new System.Collections.Generic.List<_>()
        for file in files do
            let fileContents = File.ReadAllText file
            let dir = 
                match !outputPath with
                | Some path -> path
                | _ ->
                    let dir = Path.GetDirectoryName file
                    outputPath := Some dir
                    dir

            let res = check checker options file fileContents
            for implFile in res.AssemblyContents.ImplementationFiles do
              for decl in implFile.Declarations do
                  (* failwithf "%A" decl *)
                  let modules = processDecl decl
                  for n, m in modules do
                      (* printfn "final ast: %A" m *)
                      cerl.prt m 
                      |> writeCoreFile dir n
                      |> outFiles.Add

        if not noBeam then
            match !outputPath with
            | Some outputPath -> erlc outputPath outFiles
            | _ -> failwithf "Output path was not set."
        0
    with
    | exn ->
        printfn "fez failed with:" // TODO: write in red to stderr
        printfn "%s" exn.Message
        Environment.Exit 1
        1
