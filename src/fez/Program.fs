module Fez.Program

open System
open System.Reflection
open System.IO
open Fez.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.SourceCodeServices.BasicPatterns

type FezCompileOpts =
    { OutputPath : string option
      NoBeam : bool
      Files : string list
    }

type FezCmd =
    | Help
    | Compile of FezCompileOpts


module Args =
    let (|Contains|_|) what l =
        if List.contains what l then
            Some ()
        else None

    let parseCompile argv =
        let rec inner opts =
            function
            | ("-o" | "--output") :: outPath :: tail ->
                let di = DirectoryInfo outPath
                if not di.Exists then
                    di.Create()
                inner {opts with OutputPath = Some outPath } tail
            | "--nobeam" :: tail ->
                inner {opts with NoBeam = true } tail
            | files ->
                {opts with Files = files }

        inner { OutputPath = None
                NoBeam = false
                Files = [] } argv
        |> Compile


    let parse args =
        match Array.toList args with
        | ("compile" | "c")  :: args ->
            parseCompile args
        | _ ->
            Help

    let help = """
USAGE:
    fez compile <options> <files>

        Compiles fsharp files to core erlang and beam.

        OPTIONS:
            -o | --output       set output directory
            --nobeam            do not compile core files to beam

    fez help
        Shows this text.
"""


[<EntryPoint>]
let main argv =
    try
        match Args.parse argv with
        | Help ->
            printfn "%s" Args.help
            0
        | Compile { Files = [] } ->
            eprintfn "fez compile: no files specificed!"
            printfn "%s" Args.help
            0
        | Compile { Files = files
                    OutputPath = outPath
                    NoBeam = noBeam } as c ->
            let outDir =
                match outPath with
                | None ->
                    Path.GetFullPath(Path.GetDirectoryName(files.[0]))
                | Some o -> o

            let checker = FSharpChecker.Create (keepAssemblyContents = true)
            let options = projectOptions checker files
            let outFiles =
                [ for file in files do
                    let fileContents = File.ReadAllText file
                    let res = check checker options file fileContents
                    for i in res.AssemblyContents.ImplementationFiles do
                        for decl in i.Declarations do
                            yield! doDecl decl ]
                |> List.groupBy (fun fd -> fd.Module)
                |> List.map (fun (modName, fds) ->
                    let m = toModule modName fds
                    cerl.prt m
                    |> Erlc.writeErlangCoreFile outDir modName)

            if not noBeam then
                Erlc.call outDir outFiles
            0
    with
    | exn ->
        printfn "fez failed with:" // TODO: write in red to stderr
        eprintfn "%s" exn.Message
        eprintfn "%A" exn.InnerException
        eprintfn "%A" exn.StackTrace
        Environment.Exit 1
        1
