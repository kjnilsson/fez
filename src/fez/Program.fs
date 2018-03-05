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
      DumpAst : bool
      Files : string list
    }

type ErlOpts =
    { Args : string list }

type FezCmd =
    | Init
    | Help
    | Compile of FezCompileOpts
    | Erl of ErlOpts

let projectTemplate = """<Project Sdk="Microsoft.NET.Sdk">

  <PropertyGroup>
    <TargetFramework>netstandard2.0</TargetFramework>
  </PropertyGroup>

  <ItemGroup>
    <Compile Include="code.fs" />
  </ItemGroup>

  <ItemGroup>
    <Reference Include="{{FEZ_CORE}}" />
  </ItemGroup>

</Project>
"""

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
            | ("-a" | "--dump-ast") :: tail ->
                inner {opts with DumpAst = true } tail
            | files ->
                {opts with Files = files }

        inner { OutputPath = None
                NoBeam = false
                DumpAst = false
                Files = [] } argv
        |> Compile


    let parse args =
        match Array.toList args with
        | ("compile" | "c")  :: args ->
            parseCompile args
        | "erl" :: args ->
            Erl {Args = args}
        | "init" :: args ->
            Init
        | _ ->
            Help


    let help = """
USAGE:
    fez init
        Creates a dotnet core 2.0.0 fsharp project file in the current directory
        with a reference to the Fez.Core library. Requires dotnet to be installed
        and available in PATH.

    fez compile <options> <files>
        Compiles fsharp files to core erlang and beam.

        OPTIONS:
            -o | --output       set output directory
            --nobeam            do not compile core files to beam
            -a | --dump-ast     prints the fsharp typed AST to stdout

    fez erl <args>
        Launches the erlang shell with the appropriate search paths set.

    fez help
        Shows this text.
"""


let codeTempl = """module code
open Fez.Core
"""


let relFromFez p =
    let t = typeof<FezCmd>
    let loc = Path.GetDirectoryName(t.Assembly.Location)
    Path.Combine(loc, p)

let runW p a =
    let proc = new System.Diagnostics.Process()
    proc.StartInfo.UseShellExecute <- false
    proc.StartInfo.CreateNoWindow <- true
    proc.StartInfo.FileName <- p
    proc.StartInfo.Arguments <- a
    proc.Start() |> ignore
    proc.WaitForExit()

[<EntryPoint>]
let main argv =
    try
        match Args.parse argv with
        | Init ->
            // find name of current directory
            let curDir = Directory.GetCurrentDirectory()
            let dirName = Path.GetFileName(curDir)
            let parentDir = Directory.GetParent curDir
            let fezCorePath = relFromFez "Fez.Core.dll"
            // make replacements and create project file
            let t = projectTemplate.Replace("{{FEZ_CORE}}", fezCorePath)
            let p = Path.Combine(curDir, dirName + ".fsproj")
            printfn "fez init: %s\n" p
            File.WriteAllText (p, t)
            // create code file
            File.WriteAllText (Path.Combine(curDir, "code.fs"), codeTempl)
            // run dotnet restore && dotnet build
            printfn "fez init: running dotnet restore\n"
            runW "dotnet" ("restore " + p)
            0
        | Help ->
            printfn "%s" Args.help
            0
        | Erl { Args = args } ->
            let mutable sargs = " "
            for a in args do
                sargs <- sargs + sprintf " \"%s\"" a
            let ebin = relFromFez "ebin"
            if Directory.Exists ebin then
                sargs <- sargs + sprintf " -pa \"%s\"" ebin
            else
                eprintfn "WARN: fez ebin directory %s not found\n" ebin
            runW "erl" sargs
            0
        | Compile { Files = [] } ->
            eprintfn "fez compile: no files specificed!"
            printfn "%s" Args.help
            0
        | Compile { Files = files
                    OutputPath = outPath
                    NoBeam = noBeam
                    DumpAst = dumpAst } as c ->
            let outDir =
                match outPath with
                | None ->
                    Path.GetDirectoryName(Path.GetFullPath(files.[0]))
                | Some o -> o

            let checker = FSharpChecker.Create (keepAssemblyContents = true)
            let options = projectOptions checker files
            let outFiles =
                [ for file in files do
                    let fileContents = File.ReadAllText file
                    let res = check checker options file fileContents
                    for i in res.AssemblyContents.ImplementationFiles do
                        for decl in i.Declarations do
                            if dumpAst then
                                printfn "AST: %A" decl
                            yield! doDecl decl ]
                |> List.groupBy (fun fd -> fd.Module)
                |> List.map (fun (modName, fds) ->
                    let m = toModule modName fds
                    printfn "fez: writing core module '%s'" modName
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
